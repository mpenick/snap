// Frames, upvals and the bytecode interpreter
#include "snap_internal.h"

#define relocate(stack_old, stack_new, stack_curr) \
  stack_curr = ((stack_new) + ((stack_curr) - (stack_old)))

static SValue exec(Snap* snap);

/* VM */

// Stack layout
// [args][locals][stack...]

// During call
// 1) [args and locals][stack...][calling args][function]
// 2) [args and locals][stack...][args and locals][stack...]
// 3) [args and locals][stack...][return value]

// Grows the stack so that `needed` values fit above stack_base, and returns
// where stack_base is now, or NULL past SNAP_MAX_STACK. Kept out of
// push_frame() so that it's inlined.
static __attribute__((noinline)) SValue* grow_stack(Snap* snap, SValue* stack_base, int needed) {
  SnapFrame* frame;
  int offset = stack_base - snap->stack;
  int stack_size = snap->stack_size;
  SValue* stack_old = snap->stack;
  SValue* stack_new;
  SUpval* upval;

  if (offset + needed >= SNAP_MAX_STACK) return NULL;
  while (offset + needed >= stack_size) {
    stack_size *= (stack_size < 4096 ? 4 : 2);
  }
  if (stack_size > SNAP_MAX_STACK) stack_size = SNAP_MAX_STACK;
  stack_new = (SValue*)realloc(snap->stack, stack_size * sizeof(SValue));
  snap->stack_size = stack_size;
  snap->stack = stack_new;
  snap->stack_end = stack_new + stack_size;

  // Update stack pointers to the new stack
  mvec_foreach(&snap->frames, frame) {
    relocate(stack_old, stack_new, frame->stack_base);
    relocate(stack_old, stack_new, frame->stack_top);
  }
  for (upval = snap->open_upvals; upval; upval = upval->next) {
    relocate(stack_old, stack_new, upval->value);
  }
  return stack_new + offset;
}

// Returns false, without pushing it, on a stack overflow
static inline bool push_frame(Snap* snap, SCode* code, SValue* stack_base, SClosure* closure) {
  SnapFrame* frame;
  SValue* val;
  int needed = code->num_locals + code->max_stack_size;

  if (stack_base + needed >= snap->stack_end) {
    stack_base = grow_stack(snap, stack_base, needed);
    if (!stack_base) return false;
  }

  frame = mvec_push(&snap->frames, SnapFrame);
  frame->code = code;
  frame->closure = closure;
  frame->pc = code->insts;
  frame->stack_base = stack_base;
  frame->recover = false;
  frame->stop = false;
  frame->stack_top = stack_base + code->num_locals;

  // Non-argument locals must not be garbage when the GC scans the stack
  for (val = stack_base + code->num_args; val < frame->stack_top; ++val) {
    *val = create_nil();
  }
  return true;
}

// Find or create the open upvalue for a stack slot. The open list is sorted by
// descending address so that every closure capturing a slot shares one upvalue.
static SUpval* upval_find(Snap* snap, SValue* slot) {
  SUpval** pos = &snap->open_upvals;
  SUpval* upval;
  while (*pos && (*pos)->value > slot) {
    pos = &(*pos)->next;
  }
  if (*pos && (*pos)->value == slot) {
    return *pos;
  }
  upval = (SUpval*)gc_new(snap, STYPE_UPVAL, sizeof(SUpval));
  upval->value = slot;
  upval->closed = create_nil();
  upval->next = *pos;
  *pos = upval;
  return upval;
}

// Close every open upvalue that points at or above `level`
static void close_upvals(Snap* snap, SValue* level) {
  while (snap->open_upvals && snap->open_upvals->value >= level) {
    SUpval* upval = snap->open_upvals;
    upval->closed = *upval->value;
    upval->value = &upval->closed;
    snap->open_upvals = upval->next;
  }
}

#define operand(n) ((n) > 0 ? &locals[(n) - 1] : &constants[-(n) - 1])

// Right operand first: when both come from the stack it's on top
#define binary_operands() \
  b = OPERAND_RIGHT(inst) ? operand(OPERAND_RIGHT(inst)) : --stack; \
  a = OPERAND_LEFT(inst) ? operand(OPERAND_LEFT(inst)) : --stack;

// Floats that can't be immediate are allocated, so they're stored by saving
// the frame, calling this and reloading the frame. Keeping the VM's registers
// live across the call instead would spill them in every handler.
static void store_float(Snap* snap, SnapFloat f, int dest) {
  SnapFrame* frame = &mvec_back(&snap->frames);
  SValue val = float_new(snap, f);
  if (dest) {
    frame->stack_base[dest - 1] = val;
  } else {
    *frame->stack_top++ = val;
  }
}

// Pushes (a op b), or stores it in a local. Int ops work on the tagged words
// directly. Mixed int/float operands promote to float.
#define arith_op(iop, fop) \
  if (is_int_p(a) && is_int_p(b)) { \
    uint64_t result = iop(a->bits, b->bits); \
    if (OPERAND_DEST(inst)) { \
      locals[OPERAND_DEST(inst) - 1].bits = result; \
    } else { \
      (stack++)->bits = result; \
    } \
  } else if (num_val(a, &x) && num_val(b, &y)) { \
    SnapFloat f = fop(x, y); \
    SValue val; \
    if (!snap_imm_float(f, &val)) { \
      frame->pc = pc; \
      frame->stack_top = stack; \
      store_float(snap, f, OPERAND_DEST(inst)); \
      goto new_frame; \
    } \
    if (OPERAND_DEST(inst)) { \
      locals[OPERAND_DEST(inst) - 1] = val; \
    } else { \
      *stack++ = val; \
    } \
  } else { \
    RUNTIME_ERROR("snap_type_error", "Expected numbers"); \
  }

// A compare followed by JUMP_FALSE (as in every `(if (< ...) ...)`) takes the
// branch itself rather than pushing a bool for the next instruction. Tagged
// ints order the same as their values.
#define compare_op(op) \
  if (is_int_p(a) && is_int_p(b)) { \
    cond = (SnapInt)a->bits op (SnapInt)b->bits; \
  } else if (num_val(a, &x) && num_val(b, &y)) { \
    cond = x op y; \
  } else { \
    cond = snap_compare(a, b) op 0; \
  } \
  if (INST_OP(*pc) == JUMP_FALSE) { \
    arg = INST_ARG(*pc++); \
    if (!cond) pc += arg; \
  } else { \
    *stack++ = create_bool(cond); \
  }

// i += k, then if (i op limit) jump into the loop body; otherwise fall through
// to the jump back to the loop's compare (on exit, or for non-int limits).
// The jump back is the next instruction, and the body starts two after its
// target. k is an int constant; a float i takes the slow path.
#define step_op(op) \
  a = &locals[OPERAND_DEST(inst) - 1]; \
  b = operand(OPERAND_RIGHT(inst)); \
  if (is_int_p(a)) { \
    a->bits = add_iop(a->bits, constants[-OPERAND_LEFT(inst) - 1].bits); \
    if (is_int_p(b) && (SnapInt)a->bits op (SnapInt)b->bits) { \
      pc += INST_ARG(*pc) + 3; \
    } \
  } else if (is_float(*a)) { \
    SnapFloat f = as_float(*a) + as_int(constants[-OPERAND_LEFT(inst) - 1]); \
    if (!snap_imm_float(f, a)) { \
      frame->pc = pc; \
      frame->stack_top = stack; \
      store_float(snap, f, OPERAND_DEST(inst)); \
      goto new_frame; \
    } \
  } else { \
    RUNTIME_ERROR("snap_type_error", "Expected numbers"); \
  }

// Operand fetches for the variants. Right comes first, as in binary_operands().
// A variant that falls back to the general handler skips its fetch.
#define fetch_S(n) (--stack)
#define fetch_L(n) (&locals[(n) - 1])
#define fetch_K(n) (&constants[-(n) - 1])

#define arith_variant(name, iop, ok, kl, kr) \
  OP(name##_##kl##kr) { \
    b = fetch_##kr(OPERAND_RIGHT(inst)); \
    a = fetch_##kl(OPERAND_LEFT(inst)); \
    if (is_int_p(a) && is_int_p(b) && (ok)) { \
      uint64_t result = iop(a->bits, b->bits); \
      if (OPERAND_DEST(inst)) { \
        locals[OPERAND_DEST(inst) - 1].bits = result; \
      } else { \
        (stack++)->bits = result; \
      } \
      DISPATCH(); \
    } \
    goto fetched_##name; \
  }

#define compare_variant(name, op, kl, kr) \
  OP(name##_##kl##kr) { \
    b = fetch_##kr(OPERAND_RIGHT(inst)); \
    a = fetch_##kl(OPERAND_LEFT(inst)); \
    if (is_int_p(a) && is_int_p(b)) { \
      cond = (SnapInt)a->bits op (SnapInt)b->bits; \
      if (INST_OP(*pc) == JUMP_FALSE) { \
        arg = INST_ARG(*pc++); \
        if (!cond) pc += arg; \
      } else { \
        *stack++ = create_bool(cond); \
      } \
      DISPATCH(); \
    } \
    goto fetched_##name; \
  }

#define ARITH_VARIANTS(name, iop, ok) \
  arith_variant(name, iop, ok, S, S) arith_variant(name, iop, ok, S, L) arith_variant(name, iop, ok, S, K) \
  arith_variant(name, iop, ok, L, S) arith_variant(name, iop, ok, L, L) arith_variant(name, iop, ok, L, K) \
  arith_variant(name, iop, ok, K, S) arith_variant(name, iop, ok, K, L) arith_variant(name, iop, ok, K, K)

#define COMPARE_VARIANTS(name, op) \
  compare_variant(name, op, S, S) compare_variant(name, op, S, L) compare_variant(name, op, S, K) \
  compare_variant(name, op, L, S) compare_variant(name, op, L, L) compare_variant(name, op, L, K) \
  compare_variant(name, op, K, S) compare_variant(name, op, K, L) compare_variant(name, op, K, K)

// Build with -fno-crossjumping (see Makefile): each handler must keep its own
// indirect jump so the branch predictor can learn opcode sequences
#define INDIRECT_DISPATCH 1

#ifdef INDIRECT_DISPATCH
#define OP(name) op_##name:
#define DISPATCH() \
  inst = *pc++; \
  arg = INST_ARG(inst); \
  goto *optable[INST_OP(inst)]
#else
#define OP(name) case name:
#define DISPATCH() continue
#endif

// Built-ins that the NTH, COUNT, GET, INT, MIN and MAX instructions compare
// globals with


// Unwinds a panic to the innermost frame called by recover and pushes the
// error as its result. Returns false, with this exec()'s frames gone, when
// there isn't one. Kept out of exec(), where it slows down the other handlers.
static bool unwind(Snap* snap, SValue err) {
  for (;;) {
    SnapFrame* frame = &mvec_back(&snap->frames);
    mvec_pop(&snap->frames);
    if (frame->recover) {
      close_upvals(snap, frame->stack_base);
      *mvec_back(&snap->frames).stack_top++ = err;
      return true;
    }
    if (frame->stop) {
      close_upvals(snap, frame->stack_base);
      return false;
    }
  }
}

// Replaces the last `count` values on the stack with a list of them, for a
// function whose last parameter takes the rest of its arguments, or nil if
// there are none. Returns the new top. The list is built from the end, each
// cell going in the slot of the value it took, so the GC sees all of it.
static SValue* pack_rest(Snap* snap, SValue* top, int count) {
  SValue* slot;
  *top = count > 0 ? create_empty() : create_nil(); // The callable's slot, which is free
  mvec_back(&snap->frames).stack_top = top + 1;
  for (slot = top - 1; slot >= top - count; --slot) {
    SCons* cell = cons_new(snap);
    cell->first = *slot;
    cell->rest = slot[1];
    *slot = create_obj(cell);
  }
  return (count > 0 ? top - count : top) + 1;
}

static SValue exec(Snap* snap) {
  SnapFrame* frame;
  SCode* code;
  SClosure* closure;
  SnapInst* pc;
  SnapInst inst;
  SValue* constants;
  SValue* locals;
  SValue* stack;
  SValue* a, * b;
  SnapFloat x, y; // Operands of float arithmetic and compares
  SValue temp;
  bool cond, ok;
  int arg;

#ifdef INDIRECT_DISPATCH
#define XX(opcode) [opcode] = &&op_##opcode,
  static void *optable[] = {
    OPCODE_MAP(XX)
  };
#undef XX
#endif

new_frame:
  frame = &mvec_back(&snap->frames);
  code = frame->code;
  closure = frame->closure;
  pc = frame->pc;
  constants = code->constants;
  locals = frame->stack_base;
  stack = frame->stack_top;

// Anything that can allocate must sync frame->stack_top first so the GC sees
// the whole stack. Always use inside braces.
#define RUNTIME_ERROR(err,  ...)                           \
  frame->stack_top = stack;                                \
  *stack++ = create_obj(snap_str_new_format(snap, __VA_ARGS__)); \
  frame->stack_top = stack;                                \
  *stack++ = create_obj(snap_key_new(snap, err));          \
  goto panic;

#ifdef INDIRECT_DISPATCH
  DISPATCH();
#else
  for (;;) {
    inst = *pc++;
    arg = INST_ARG(inst);
    switch (INST_OP(inst)) {
#endif
      OP(LOAD_GLOBAL) {
        SSym* sym = as_sym(constants[arg]);
        if (is_undef(sym->value)) {
          RUNTIME_ERROR("snap_name_error", "'%s' is not defined", sym->data);
        }
        *stack++ = sym->value;
        DISPATCH();
      }
      OP(STORE_GLOBAL) {
        SSym* sym = as_sym(constants[arg]);
        if (sym->read_only) { // Code compiled before the global was made a constant
          RUNTIME_ERROR("snap_type_error", "%s is a constant", sym->data);
        }
        gc_barrier(snap, sym->value);
        sym->value = *--stack;
        DISPATCH();
      }
      OP(LOAD_LOCAL) {
        *stack++ = locals[arg];
        DISPATCH();
      }
      OP(STORE_LOCAL) {
        locals[arg] = *--stack;
        DISPATCH();
      }
      OP(LOAD_CLOSED) {
        *stack++ = *closure->upvals[arg]->value;
        DISPATCH();
      }
      OP(STORE_CLOSED) {
        SValue* value = closure->upvals[arg]->value;
        gc_barrier(snap, *value);
        *value = *--stack;
        DISPATCH();
      }
      OP(LOAD_CONSTANT) {
        *stack++ = constants[arg];
        DISPATCH();
      }
      OP(LOAD_NIL) {
        *stack++ = create_nil();
        DISPATCH();
      }
      OP(POP) {
        --stack;
        DISPATCH();
      }
      OP(DUP) {
        *stack = *(stack - 1);
        stack++;
        DISPATCH();
      }
      OP(ROT2) {
        temp = *(stack - 1);
        *(stack - 1) = *(stack - 2);
        *(stack - 2) = temp;
        DISPATCH();
      }
      OP(ROT3) {
        temp = *(stack - 3);
        *(stack - 3) = *(stack - 1);
        *(stack - 1) = *(stack - 2);
        *(stack - 2) = temp;
        DISPATCH();
      }
      // (nth v i), (count x) and (get m k) are calls to globals, as with
      // CALL_GLOBAL, that do the common cases themselves while the global is
      // still the built-in
#define builtin_global(fn) (as_sym(constants[arg >> 8])->value.bits == create_cfunc(fn).bits)
      OP(NTH) {
        a = stack - 2;
        b = stack - 1;
        if (builtin_global(builtin_nth) && is_vec(*a) && is_int(*b) &&
            (uint64_t)as_int(*b) < (uint64_t)as_vec(*a)->len) {
          *a = *snap_vec_at(as_vec(*a), (int)as_int(*b));
          stack--;
          DISPATCH();
        }
        // A map's i-th entry, to a pattern that takes its key and value
        // (UNPACK 1 or 2 next): them, with no [k v] made
        if (builtin_global(builtin_nth) && is_map(*a) && is_int(*b) && INST_OP(*pc) == UNPACK && INST_ARG(*pc) <= 2) {
          SValue* e = map_entry_at(snap, as_map(*a), as_int(*b));
          if (e) {
            a[0] = e[0];
            a[1] = e[1];
            stack = a + INST_ARG(*pc);
            pc++;
            DISPATCH();
          }
        }
        goto call_global;
      }
      OP(COUNT) {
        a = stack - 1;
        if (builtin_global(builtin_count) && (is_vec(*a) || is_map(*a))) {
          *a = create_int(is_vec(*a) ? as_vec(*a)->len : as_map(*a)->len);
          DISPATCH();
        }
        goto call_global;
      }
      // GET and PUT keep their call site's field cache slot in the low 8
      // bits (in place of the number of arguments, which is fixed)
      OP(GET) {
        SValue* val;
        int i;
        a = stack - 2;
        b = stack - 1;
        if (builtin_global(builtin_get)) {
          if (is_struct(*a) && (i = cached_field(code, arg & 0xff, as_struct(*a), *b)) >= 0) {
            *a = as_struct(*a)->vals[i];
            stack--;
            DISPATCH();
          }
          if (is_map(*a)) {
            val = map_get(as_map(*a), *b);
            *a = val ? *val : create_nil();
            stack--;
            DISPATCH();
          }
        }
        arg = (arg & ~0xff) | 2;
        goto call_global;
      }
      OP(PUT) {
        int i;
        a = stack - 3;
        b = stack - 2;
        if (builtin_global(builtin_put) && is_struct(*a) && (i = cached_field(code, arg & 0xff, as_struct(*a), *b)) >= 0) {
          SValue* val = &as_struct(*a)->vals[i];
          gc_barrier(snap, *val);
          *val = *(stack - 1);
          stack -= 2; // The struct is the result
          DISPATCH();
        }
        arg = (arg & ~0xff) | 3;
        goto call_global;
      }
      OP(INT) {
        a = stack - 1;
        if (builtin_global(builtin_int) && is_imm_float(*a)) {
          x = snap_imm_float_val(*a);
          if (in_int_range(x)) {
            *a = create_int((SnapInt)x);
            DISPATCH();
          }
        }
        goto call_global;
      }
      // (min x y...) and (max x y...) of numbers. The result is the first
      // argument that's smallest (largest), as with the built-ins.
#define min_max_op(fn, op) \
        int i, n = arg & 0xff; \
        SValue* best; \
        a = stack - n; \
        if (builtin_global(fn) && num_val(a, &x)) { \
          for (best = a, i = 1; i < n; ++i) { \
            if (!num_val(a + i, &y)) goto call_global; \
            if (y op x) { \
              x = y; \
              best = a + i; \
            } \
          } \
          *a = *best; \
          stack = a + 1; \
          DISPATCH(); \
        } \
        goto call_global;
      OP(MIN) { min_max_op(builtin_min, <) }
      OP(MAX) { min_max_op(builtin_max, >) }
#undef min_max_op
#undef builtin_global
      OP(CALL_GLOBAL) {
        SSym* sym;
      call_global:
        sym = as_sym(constants[arg >> 8]);
        if (is_undef(sym->value)) {
          RUNTIME_ERROR("snap_name_error", "'%s' is not defined", sym->data);
        }
        a = stack; // Where CALL would have popped it from
        *a = sym->value;
        arg &= 0xff;
        goto call;
      }
      OP(CALL) {
        a = --stack; // Pop callable
      call:
        if (is_cfunc(*a)) {
          if (as_cfunc(*a) == builtin_apply && arg >= 2 && is_seq(stack[-1])) {
            // (apply f a b xs) becomes (f a b xs...) here, so it's an
            // ordinary call. A bad call goes to builtin_apply() for its error.
            SValue fn, seq = stack[-1];
            int len = seq_length(seq), i;
            if (stack + len + 2 >= snap->stack_end) {
              SValue* moved;
              frame->stack_top = stack;
              moved = grow_stack(snap, stack, len + 2);
              if (!moved) {
                RUNTIME_ERROR("snap_stack_error", "Stack overflow");
              }
              stack = moved;
              locals = frame->stack_base;
            }
            fn = stack[-arg];
            memmove(stack - arg, stack - arg + 1, (arg - 2) * sizeof(SValue)); // The fixed arguments
            stack -= 2;
            if (is_vec(seq)) {
              for (i = 0; i < len; ++i) *stack++ = *snap_vec_at(as_vec(seq), i);
            } else {
              for (; !is_nil(seq) && !is_empty(seq); seq = as_cons(seq)->rest) *stack++ = as_cons(seq)->first;
            }
            a = stack;
            *a = fn;
            arg = arg - 2 + len;
            goto call;
          }
          temp = create_nil();
          frame->stack_top = stack; // Keep the arguments rooted during the call
          ok = as_cfunc(*a)(snap, stack - arg, arg, &temp);
          stack -= arg; // Pop arguments
          if (snap->reentered) {
            // It ran snap code (like a macro), which can move the frames and
            // the stack
            snap->reentered = false;
            frame = &mvec_back(&snap->frames);
            locals = frame->stack_base;
            stack = frame->stack_top - arg;
            if (ok && snap->fin_pending) {
              // Structs are waiting for their finalizers (which set
              // reentered), and between instructions is a safe place to run
              // them. The result goes on the stack meanwhile, to keep it.
              *stack = temp;
              frame->stack_top = stack + 1;
              snap_run_finalizers(snap);
              snap->reentered = false;
              frame = &mvec_back(&snap->frames);
              locals = frame->stack_base;
              stack = frame->stack_top - 1;
              temp = *stack;
            }
          }
          if (!ok) goto call_failed;
          *stack++ = temp;
        } else if (is_code(*a) || is_closure(*a)) {
          SClosure* cl = is_closure(*a) ? as_closure(*a) : NULL;
          SCode* c = cl ? cl->code : as_code(*a);
          if (arg != c->arity) {
            if (c->arity >= 0 || arg < c->num_args - 1) {
              RUNTIME_ERROR("snap_arity_error", "Invalid number of arguments");
            }
            frame->stack_top = stack;
            stack = pack_rest(snap, stack, arg - (c->num_args - 1));
            arg = c->num_args;
          }
          frame->pc = pc;
          frame->stack_top = stack - arg; // Pop arguments
          if (!push_frame(snap, c, frame->stack_top, cl)) {
            RUNTIME_ERROR("snap_stack_error", "Stack overflow");
          }
          goto new_frame;
        } else if (is_method(*a)) {
          SValue* fn = method_find(snap, as_method(*a), stack - arg, arg);
          if (!fn) {
            frame->stack_top = stack;
            temp = create_obj(method_error(snap, as_method(*a), stack - arg, arg));
            goto call_failed;
          }
          *a = *fn;
          goto call;
        } else if (is_type(*a)) {
          frame->stack_top = stack; // Keeps the arguments rooted (types are never freed)
          ok = struct_new(snap, as_type(*a), stack - arg, arg, &temp);
          stack -= arg;
          if (!ok) goto call_failed;
          *stack++ = temp;
        } else if (is_key(*a)) {
          frame->stack_top = stack;
          ok = key_call(snap, *a, stack - arg, arg, &temp);
          stack -= arg;
          if (!ok) goto call_failed;
          *stack++ = temp;
        } else {
          RUNTIME_ERROR("snap_type_error", "Value is not callable");
        }
        DISPATCH();
      call_failed:
        *stack++ = as_err(temp)->msg;
        *stack++ = as_err(temp)->err;
        goto panic;
      }
      OP(CLOSURE) {
        int i;
        SCode* c = as_code(*(stack - 1));
        SClosure* cl;
        frame->stack_top = stack;
        cl = closure_new(snap, c);
        *(stack - 1) = create_obj(cl); // Replace the code, keeping the closure rooted
        for (i = 0; i < c->num_upvals; ++i) {
          uint32_t desc = c->upvals[i];
          cl->upvals[i] = (desc & 1) ? upval_find(snap, &locals[desc >> 1])
                                     : closure->upvals[desc >> 1];
        }
        DISPATCH();
      }
      OP(CLOSE_UPVALS) {
        close_upvals(snap, locals + arg);
        DISPATCH();
      }
      OP(RETURN) {
      do_return:
        close_upvals(snap, locals);
        if (!frame->stop) {
          SnapFrame* frame_prev = &mvec_back(&snap->frames) - 1;
          *frame_prev->stack_top++ = *--stack;
          mvec_pop(&snap->frames);
          goto new_frame;
        }
        temp = *--stack;
        mvec_pop(&snap->frames);
        return temp;
      }
      // Returns the vector literal [a b ...] that ends a function. A caller
      // that only takes its parts (UNPACK n next) gets them on its stack,
      // with no vector made.
      OP(RETURN_VEC) {
        stack--; // Pop the vector function
        if (!frame->stop) {
          SnapFrame* frame_prev = &mvec_back(&snap->frames) - 1;
          SnapInst next = *frame_prev->pc;
          if (INST_OP(next) == UNPACK && INST_ARG(next) <= arg) {
            close_upvals(snap, locals); // Before the parts go over the frame
            memmove(frame_prev->stack_top, stack - arg, INST_ARG(next) * sizeof(SValue));
            frame_prev->stack_top += INST_ARG(next);
            frame_prev->pc++; // Past the UNPACK
            mvec_pop(&snap->frames);
            goto new_frame;
          }
        }
        frame->stack_top = stack; // Keep the parts rooted
        vec_from_args(snap, stack - arg, arg, &temp);
        stack -= arg;
        *stack++ = temp;
        goto do_return;
      }
      // Replaces a vector (or list) with its first n items, for a pattern
      // that takes its parts. Short or not a vector is nth's error.
      OP(UNPACK) {
        int i;
        a = stack - 1;
        if (is_vec(*a) && as_vec(*a)->len >= arg) {
          SVec* v = as_vec(*a);
          for (i = 0; i < arg; ++i) a[i] = *snap_vec_at(v, i);
          stack = a + arg;
          DISPATCH();
        }
        // The parts go above the value, which stays rooted, then move down
        for (i = 0; i < arg; ++i) {
          SValue nth_args[2];
          nth_args[0] = *a;
          nth_args[1] = create_int(i);
          frame->stack_top = stack;
          if (!builtin_nth(snap, nth_args, 2, &temp)) goto call_failed;
          *stack++ = temp;
        }
        memmove(a, a + 1, arg * sizeof(SValue));
        stack--;
        DISPATCH();
      }
      OP(JUMP) {
        pc += arg;
        DISPATCH();
      }
      OP(JUMP_FALSE) {
        if (is_falsy(*--stack)) pc += arg;
        DISPATCH();
      }
      OP(JUMP_TRUE) {
        if (!is_falsy(*--stack)) pc += arg;
        DISPATCH();
      }
      OP(JUMP_FALSE_OR_POP) {
        if (is_falsy(*(stack - 1))) {
          pc += arg;
        } else {
          --stack;
        }
        DISPATCH();
      }
      OP(JUMP_TRUE_OR_POP) {
        if (!is_falsy(*(stack - 1))) {
          pc += arg;
        } else {
          --stack;
        }
        DISPATCH();
      }
      OP(TRY) {
        // An error returns from the function, otherwise it's the value
        if (is_err(*(stack - 1))) goto do_return;
        DISPATCH();
      }
      OP(RECOVER) {
        // Calls a function without arguments. If it panics, the error is the
        // result instead (see panic below).
        a = --stack; // Pop callable
        if (is_cfunc(*a)) {
          temp = create_nil();
          frame->stack_top = stack;
          as_cfunc(*a)(snap, stack, 0, &temp); // On failure temp is the error
          frame = &mvec_back(&snap->frames);
          locals = frame->stack_base;
          stack = frame->stack_top;
          *stack++ = temp;
        } else if (is_code(*a) || is_closure(*a)) {
          SClosure* cl = is_closure(*a) ? as_closure(*a) : NULL;
          SCode* c = cl ? cl->code : as_code(*a);
          if (c->arity != 0) {
            RUNTIME_ERROR("snap_arity_error", "recover expects a function without arguments");
          }
          frame->pc = pc;
          frame->stack_top = stack;
          if (!push_frame(snap, c, stack, cl)) {
            RUNTIME_ERROR("snap_stack_error", "Stack overflow");
          }
          mvec_back(&snap->frames).recover = true;
          goto new_frame;
        } else {
          RUNTIME_ERROR("snap_type_error", "Value is not callable");
        }
        DISPATCH();
      }
      OP(PANIC) {
        goto panic;
      }
      OP(NOT) {
        *(stack - 1) = create_bool(is_falsy(*(stack - 1)));
        DISPATCH();
      }
      // The general handlers, which the variants fall back to for anything but
      // ints (and division by zero)
      OP(ADD) { binary_operands() fetched_ADD: arith_op(add_iop, add_fop) DISPATCH(); }
      OP(SUB) { binary_operands() fetched_SUB: arith_op(sub_iop, sub_fop) DISPATCH(); }
      OP(MUL) { binary_operands() fetched_MUL: arith_op(mul_iop, mul_fop) DISPATCH(); }
      OP(DIV) {
        binary_operands()
      fetched_DIV:
        if (int_zero_divisor()) {
          RUNTIME_ERROR("snap_arith_error", "Division by zero");
        }
        arith_op(div_iop, div_fop)
        DISPATCH();
      }
      OP(MOD) {
        binary_operands()
      fetched_MOD:
        if (int_zero_divisor()) {
          RUNTIME_ERROR("snap_arith_error", "Division by zero");
        }
        arith_op(mod_iop, mod_fop)
        DISPATCH();
      }
      OP(STEP_LT) { step_op(<) DISPATCH(); }
      OP(STEP_LE) { step_op(<=) DISPATCH(); }
      OP(STEP_GT) { step_op(>) DISPATCH(); }
      OP(STEP_GE) { step_op(>=) DISPATCH(); }
      OP(LESS_THAN)      { binary_operands() fetched_LESS_THAN: compare_op(<) DISPATCH();  }
      OP(GREATER_THAN)   { binary_operands() fetched_GREATER_THAN: compare_op(>) DISPATCH();  }
      OP(LESS_EQUALS)    { binary_operands() fetched_LESS_EQUALS: compare_op(<=) DISPATCH(); }
      OP(GREATER_EQUALS) { binary_operands() fetched_GREATER_EQUALS: compare_op(>=) DISPATCH(); }
      OP(EQUALS)         { binary_operands() fetched_EQUALS: compare_op(==) DISPATCH(); }

      // The variants: operands fetched without decoding, ints only
      ARITH_VARIANTS(ADD, add_iop, true)
      ARITH_VARIANTS(SUB, sub_iop, true)
      ARITH_VARIANTS(MUL, mul_iop, true)
      ARITH_VARIANTS(DIV, div_iop, b->bits != 1) // Tagged 0 is 1
      ARITH_VARIANTS(MOD, mod_iop, b->bits != 1)
      COMPARE_VARIANTS(LESS_THAN, <)
      COMPARE_VARIANTS(LESS_EQUALS, <=)
      COMPARE_VARIANTS(GREATER_THAN, >)
      COMPARE_VARIANTS(GREATER_EQUALS, >=)
      COMPARE_VARIANTS(EQUALS, ==)
#ifndef INDIRECT_DISPATCH
    }
  }
#endif

#undef RUNTIME_ERROR

panic:
  // Stack: [... msg err]. Allocate the error while the stack is still rooted.
  frame->stack_top = stack;
  temp = create_obj(snap_err_new(snap, *(stack - 1), *(stack - 2)));
  if (unwind(snap, temp)) goto new_frame;
  return temp;
}

SValue snap_run(Snap* snap, SValue fn) {
  return snap_call(snap, fn, NULL, 0);
}

SValue snap_call(Snap* snap, SValue fn, const SValue* args, int num_args) {
  SValue* base = snap->frames.vsize > 0 ? mvec_back(&snap->frames).stack_top : snap->stack;
  SValue rest = create_empty(), result;
  SnapList list = { NULL, NULL };
  SnapFrame* frame;
  SClosure* cl;
  SCode* c;
  ptrdiff_t offset;
  int i, n = num_args;

  if (is_method(fn)) {
    SValue* found = method_find(snap, as_method(fn), args, num_args);
    if (!found) return create_obj(method_error(snap, as_method(fn), args, num_args));
    fn = *found;
  }
  if (is_cfunc(fn) || is_type(fn) || is_key(fn)) {
    result = create_nil();
    // On failure result is the error
    if (is_cfunc(fn)) as_cfunc(fn)(snap, args, num_args, &result);
    else if (is_type(fn)) struct_new(snap, as_type(fn), args, num_args, &result);
    else key_call(snap, fn, args, num_args, &result);
    return result;
  }
  if (!is_code(fn) && !is_closure(fn)) {
    return create_obj(snap_err_new_format(snap, "snap_type_error", "Value is not callable"));
  }
  cl = is_closure(fn) ? as_closure(fn) : NULL;
  c = cl ? cl->code : as_code(fn);
  if (num_args != c->arity) {
    if (c->arity >= 0 || num_args < c->num_args - 1) {
      return create_obj(snap_err_new_format(snap, "snap_arity_error", "Invalid number of arguments"));
    }
    n = c->num_args - 1;
    for (i = n; i < num_args; ++i) *list_add(snap, &list) = args[i];
    rest = num_args > n ? list_done(snap, &list) : create_nil(); // Rooted again by the frame before anything allocates
  }

  // The arguments can be in the stack, which push_frame() can move
  offset = args >= snap->stack && args < snap->stack_end ? args - snap->stack : -1;
  if (snap->c_depth >= SNAP_MAX_C_DEPTH || !push_frame(snap, c, base, cl)) {
    return create_obj(snap_err_new_format(snap, "snap_stack_error", "Stack overflow"));
  }
  if (offset >= 0) args = snap->stack + offset;
  frame = &mvec_back(&snap->frames);
  if (n > 0) memmove(frame->stack_base, args, n * sizeof(SValue));
  if (c->arity < 0) frame->stack_base[n] = rest;
  frame->stop = true;
  snap->c_depth++;
  result = exec(snap);
  snap->c_depth--;
  snap->reentered = true; // For a VM that called this through a C function
  return result;
}
