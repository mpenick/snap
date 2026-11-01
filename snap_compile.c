// The compiler, from values to bytecode, and the disassembler
#include "snap_compile.h"
#include "snap_vm.h"
#include "snap_object.h"
#include "snap_builtins.h"

/* Compiler */

#define MAX_LOCALS 256
#define MAX_UPVALS 256
#define MAX_INLINE_ARGS 8 // Functions inline_global() inlines: up to this many parameters
#define INLINE_SIZE 40    // and values in their body (as written, and
                          // instructions once compiled, macros expanded)

static void print_code(SCode* code, int indent);

// Compile-time state is plain malloc'd memory, not GC objects. The only GC
// values it holds are constants, which are rooted through snap->fn_state.
// Symbols are interned and live forever, so raw SSym* are safe to keep.

typedef struct SnapLocal_ {
  SSym* name;
  int depth;
  bool captured; // Closed over by an inner fn, through an upvalue
  bool copied;   // Closed over by an inner fn, by value
  bool assigned; // A set! of it (maybe), so it can't be copied
  int alias;     // The local it names instead of its own slot, or -1: an
                 // inlined function's parameter, passed a local
  int constant;  // Or the constant it names, passed a literal, or -1
} SnapLocal;

// The target of a recur: a loop, or the whole fn
typedef struct SnapLoop_ {
  int top;     // Instruction to jump back to
  int base;    // Slot of the first variable
  int count;   // Number of variables
  bool closes; // The body has a fn, so recur must close upvalues first
  struct SnapLoop_* up;
} SnapLoop;

typedef struct SnapFnState_ {
  Snap* snap;
  struct SnapFnState_* up;
  struct { MVEC_FIELDS(SnapInst); } insts;
  SnapVec constants;
  SnapLocal locals[MAX_LOCALS];
  int num_locals;
  int max_locals;
  uint32_t upvals[MAX_UPVALS]; // (index << 2) | copied << 1 | islocal
  int num_upvals;
  SCons* plain_call; // The call inline_seq() falls back to, compiled as a call
  SSym** sets;   // Every name a set! in the function's body names (see scan_sets)
  int num_sets;
  int sets_capacity;
  int num_args;
  bool variadic;   // The last parameter takes the rest of the arguments
  bool top;        // Code from snap_compile(), which can't see the locals of
                   // the function whose macro is compiling it
  int depth;       // Scope depth; 0 is top-level, where def makes globals
  int stack;       // Operand stack depth at this point in the code
  int max_stack;
  int last_target; // Instruction most recently targeted by a jump
  int num_caches;  // Field caches for GET and PUT
  SnapLoop* loop;  // Innermost recur target, NULL at the top-level
  SnapLoop fn_loop;
  int inline_form; // See SCode
  int barrier;     // Locals below it can't be seen: an inlined function's
  int inlining;    // body is being compiled (inside this many)
  SCode* inlined[2]; // and these are the functions
} SnapFnState;

void gc_mark_fn_states(Snap* snap) {
  SnapFnState* fs;
  for (fs = snap->fn_state; fs; fs = fs->up) {
    gc_mark_vals(snap, fs->constants.vitems, fs->constants.vsize);
  }
}

static SnapFnState* fn_state_new(Snap* snap) {
  SnapFnState* fs = (SnapFnState*)calloc(1, sizeof(SnapFnState));
  mvec_init(&fs->insts, SnapInst, 16);
  mvec_init(&fs->constants, SValue, 8);
  fs->last_target = -1;
  fs->inline_form = -1;
  fs->snap = snap;
  fs->up = snap->fn_state;
  snap->fn_state = fs;
  return fs;
}

static void fn_state_free(Snap* snap) {
  SnapFnState* fs = snap->fn_state;
  snap->fn_state = fs->up;
  mvec_destroy(&fs->insts);
  mvec_destroy(&fs->constants);
  free(fs->sets);
  free(fs);
}

// Switches binary ops to the variant for their operand kinds. Done last, so
// the compiler only deals with the base ops.
static void specialize(SnapFnState* fs) {
  int i;
  for (i = 0; i < fs->insts.vsize; ++i) {
    SnapInst* inst = &fs->insts.vitems[i];
    int op = INST_OP(*inst);
    if (is_binary_op(op)) *inst = (*inst & ~0xFFu) | variant(op, *inst);
  }
}

// Whether the code from at returns what's on top of the stack, without
// changing it: a RETURN, maybe after closing upvalues or jumping to one
static bool returns_top(SnapFnState* fs, int at) {
  int steps;
  for (steps = 0; steps < 16 && at < fs->insts.vsize; ++steps) {
    SnapInst inst = fs->insts.vitems[at];
    if (INST_OP(inst) == RETURN) return true;
    if (INST_OP(inst) == CLOSE_UPVALS) at++;
    else if (INST_OP(inst) == JUMP) at += 1 + INST_ARG(inst);
    else return false;
  }
  return false;
}

// A vector literal that's returned, [a b] at the end of a function, returns
// with RETURN_VEC, which can hand its parts to a caller that takes them apart
static void return_vectors(SnapFnState* fs) {
  int i;
  for (i = 1; i < fs->insts.vsize; ++i) {
    SnapInst inst = fs->insts.vitems[i], prev = fs->insts.vitems[i - 1];
    if (INST_OP(inst) == CALL && INST_OP(prev) == LOAD_CONSTANT &&
        fs->constants.vitems[INST_ARG(prev)].bits == create_cfunc(builtin_vector).bits && returns_top(fs, i + 1)) {
      fs->insts.vitems[i] = INST(RETURN_VEC, INST_ARG(inst));
    }
  }
}

// A jump to a JUMP goes straight to where that one goes, as the branches of
// an if that's a value do (each jumps past the other, often to another jump)
static void thread_jumps(SnapFnState* fs) {
  int i;
  for (i = 0; i < fs->insts.vsize; ++i) {
    SnapInst* inst = &fs->insts.vitems[i];
    int op = INST_OP(*inst), target, steps;
    if (op != JUMP && op != JUMP_FALSE && op != JUMP_TRUE && op != JUMP_FALSE_OR_POP && op != JUMP_TRUE_OR_POP) continue;
    target = i + 1 + INST_ARG(*inst);
    for (steps = 0; steps < 8 && target < fs->insts.vsize && INST_OP(fs->insts.vitems[target]) == JUMP; ++steps) {
      int next = target + 1 + INST_ARG(fs->insts.vitems[target]);
      if (next == target) break;
      target = next;
    }
    *inst = INST(op, target - (i + 1));
  }
}

static SCode* code_new(Snap* snap, SnapFnState* fs) {
  SCode* c = (SCode*)gc_new(snap, STYPE_CODE, sizeof(SCode));
  return_vectors(fs);
  thread_jumps(fs);
  specialize(fs);
  c->insts = fs->insts.vitems;
  c->insts_count = fs->insts.vsize;
  c->constants = fs->constants.vitems;
  c->num_constants = fs->constants.vsize;
  c->upvals = (uint32_t*)malloc(fs->num_upvals * sizeof(uint32_t));
  memcpy(c->upvals, fs->upvals, fs->num_upvals * sizeof(uint32_t));
  c->num_upvals = fs->num_upvals;
  c->num_args = fs->num_args;
  c->arity = fs->variadic ? -1 : fs->num_args;
  c->num_locals = fs->max_locals;
  c->max_stack_size = fs->max_stack + 2; /* Add enough room for raising errors */
  c->num_caches = fs->num_caches;
  c->caches = (SFieldCache*)calloc(fs->num_caches > 0 ? fs->num_caches : 1, sizeof(SFieldCache));
  c->inline_form = fs->inline_form;
  heap_add(snap, code_extra_bytes(c));

  // The code object owns the buffers now
  fs->insts.vitems = NULL;
  fs->insts.vsize = 0;
  fs->constants.vitems = NULL;
  fs->constants.vsize = 0;
  return c;
}

static void adjust_stack(SnapFnState* fs, int n) {
  fs->stack += n;
  if (fs->stack > fs->max_stack) {
    fs->max_stack = fs->stack;
  }
}

static int pc(SnapFnState* fs) {
  return fs->insts.vsize;
}

static int last_op(SnapFnState* fs) {
  return pc(fs) > 0 ? (int)INST_OP(fs->insts.vitems[pc(fs) - 1]) : -1;
}

// If the last instruction loads a local or constant, remove it and return it
// as a binary op operand, otherwise 0 (the stack)
static int take_operand(SnapFnState* fs) {
  int arg, operand;
  if (fs->last_target == pc(fs) || pc(fs) == 0) return 0;
  arg = INST_ARG(fs->insts.vitems[pc(fs) - 1]);
  switch (last_op(fs)) {
    case LOAD_LOCAL:
      if (arg + 1 > MAX_OPERAND) return 0;
      operand = arg + 1;
      break;
    case LOAD_CONSTANT:
      if (arg > MAX_OPERAND) return 0;
      operand = -(arg + 1);
      break;
    default:
      return 0;
  }
  fs->insts.vsize--;
  adjust_stack(fs, -1);
  return operand;
}

static int add_constant(Snap* snap, SnapFnState* fs, SValue val);

// Works out (op a b) of two number constants now, the way the instruction
// would, and loads the result. Division and modulo are left to run, for
// their errors.
static bool fold(SnapFnState* fs, int op, int left, int right) {
  const SValue* a, * b;
  SValue val;
  SnapFloat x, y;
  if (left >= 0 || right >= 0 || op == DIV || op == MOD) return false;
  a = &fs->constants.vitems[-left - 1];
  b = &fs->constants.vitems[-right - 1];
  if (is_int_p(a) && is_int_p(b)) {
    switch (op) {
      case ADD: val.bits = add_iop(a->bits, b->bits); break;
      case SUB: val.bits = sub_iop(a->bits, b->bits); break;
      case MUL: val.bits = mul_iop(a->bits, b->bits); break;
      case LESS_THAN: val = create_bool((SnapInt)a->bits < (SnapInt)b->bits); break;
      case LESS_EQUALS: val = create_bool((SnapInt)a->bits <= (SnapInt)b->bits); break;
      case GREATER_THAN: val = create_bool((SnapInt)a->bits > (SnapInt)b->bits); break;
      case GREATER_EQUALS: val = create_bool((SnapInt)a->bits >= (SnapInt)b->bits); break;
      default: val = create_bool(a->bits == b->bits); break;
    }
  } else if (num_val(a, &x) && num_val(b, &y)) {
    switch (op) {
      case ADD: val = float_new(fs->snap, x + y); break;
      case SUB: val = float_new(fs->snap, x - y); break;
      case MUL: val = float_new(fs->snap, x * y); break;
      case LESS_THAN: val = create_bool(x < y); break;
      case LESS_EQUALS: val = create_bool(x <= y); break;
      case GREATER_THAN: val = create_bool(x > y); break;
      case GREATER_EQUALS: val = create_bool(x >= y); break;
      default: val = create_bool(x == y); break;
    }
  } else {
    return false;
  }
  *mvec_push(&fs->insts, SnapInst) = INST(LOAD_CONSTANT, add_constant(fs->snap, fs, val));
  adjust_stack(fs, 1);
  return true;
}

static int emit_binary_operands(SnapFnState* fs, int op, int left, int right) {
  if (fold(fs, op, left, right)) return pc(fs) - 1;
  adjust_stack(fs, 1 - (left == 0) - (right == 0)); // Pops stack operands, pushes the result
  *mvec_push(&fs->insts, SnapInst) = INST(op, OPERANDS(0, left, right));
  return pc(fs) - 1;
}

static int emit_binary(SnapFnState* fs, int op) {
  int right = take_operand(fs);
  int left = right ? take_operand(fs) : 0; // Only adjacent if right was a load too
  return emit_binary_operands(fs, op, left, right);
}

// The instruction for a call to a global: CALL_GLOBAL, or one of the
// instructions for built-ins that do their common cases themselves
static int call_op(SnapFnState* fs, int index, int num_args) {
  const char* name = as_sym(fs->constants.vitems[index])->data;
  if (num_args == 2 && strcmp(name, "nth") == 0) return NTH;
  if (num_args == 1 && strcmp(name, "count") == 0) return COUNT;
  if (num_args == 2 && strcmp(name, "get") == 0) return GET;
  if (num_args == 3 && strcmp(name, "put!") == 0) return PUT;
  if (num_args == 1 && strcmp(name, "int") == 0) return INT;
  if (num_args >= 1 && strcmp(name, "min") == 0) return MIN;
  if (num_args >= 1 && strcmp(name, "max") == 0) return MAX;
  return CALL_GLOBAL;
}

static int emit(SnapFnState* fs, int op, int arg) {
  if (is_binary_op(op)) {
    return emit_binary(fs, op);
  }

  // Peepholes, unless a jump lands between the instructions
  if (fs->last_target != pc(fs)) {
    int last = last_op(fs);
    if (op == POP && (last == LOAD_LOCAL || last == LOAD_CLOSED || last == LOAD_COPIED ||
                      last == LOAD_CONSTANT || last == LOAD_NIL)) {
      fs->insts.vsize--;
      adjust_stack(fs, -1);
      return pc(fs) - 1;
    }
    // Calls to a global take the function's name with them: the constant's
    // index in the high 16 bits, the number of arguments in the low 8
    if (op == CALL && last == LOAD_GLOBAL && arg < 256) {
      SnapInst* inst = &fs->insts.vitems[pc(fs) - 1];
      if (INST_ARG(*inst) < 32768) {
        int op = call_op(fs, INST_ARG(*inst), arg), low = arg;
        if (op == GET || op == PUT) low = fs->num_caches < 255 ? ++fs->num_caches : 0; // Its cache slot
        *inst = INST(op, INST_ARG(*inst) << 8 | low);
        adjust_stack(fs, -1 - arg); // Pops the arguments and the function, pushes the result
        return pc(fs) - 1;
      }
    }
    if (op == RETURN && last == CLOSE_UPVALS) {
      fs->insts.vsize--; // RETURN closes all of the frame's upvalues anyway
    }
    // Arithmetic stores its result in the local itself
    if (op == STORE_LOCAL && is_arith_op(last) && arg + 1 <= MAX_DEST) {
      SnapInst* inst = &fs->insts.vitems[pc(fs) - 1];
      if (OPERAND_DEST(*inst) == 0) {
        *inst |= (uint32_t)(arg + 1) << 24;
        adjust_stack(fs, -1);
        return pc(fs) - 1;
      }
    }
  }

  switch (op) {
    case LOAD_GLOBAL: case LOAD_LOCAL: case LOAD_CLOSED: case LOAD_COPIED:
    case LOAD_CONSTANT: case LOAD_NIL: case DUP:
      adjust_stack(fs, 1);
      break;
    case STORE_GLOBAL: case STORE_LOCAL: case STORE_CLOSED:
    case POP: case RETURN: case PANIC:
    case JUMP_FALSE: case JUMP_TRUE: case JUMP_FALSE_OR_POP: case JUMP_TRUE_OR_POP:
      adjust_stack(fs, -1);
      break;
    case CALL:
      adjust_stack(fs, -arg); // subtract arg + 1, add 1
      break;
  }

  *mvec_push(&fs->insts, SnapInst) = INST(op, arg);
  return pc(fs) - 1;
}

static int emit_jump(SnapFnState* fs, int op) {
  return emit(fs, op, 0);
}

// Jump offsets are relative to the instruction after the jump
static void patch_jump(Snap* snap, SnapFnState* fs, int at) {
  int offset = pc(fs) - (at + 1);
  if (offset > MAX_ARG) {
    parse_raise(snap, "snap_compile_error", "Jump is too far");
  }
  fs->insts.vitems[at] = INST(INST_OP(fs->insts.vitems[at]), offset);
  fs->last_target = pc(fs);
}

static void emit_jump_back(Snap* snap, SnapFnState* fs, int target) {
  int offset = target - (pc(fs) + 1);
  if (offset < -MAX_ARG) {
    parse_raise(snap, "snap_compile_error", "Jump is too far");
  }
  emit(fs, JUMP, offset);
}

static int add_global_constant(Snap* snap, SnapFnState* fs, SValue val);

static int add_constant(Snap* snap, SnapFnState* fs, SValue val) {
  int i;
  // Floats by identity: compared, 0.0 equals -0.0, and NaN equals anything
  if (is_float(val)) return add_global_constant(snap, fs, val);
  for (i = 0; i < fs->constants.vsize; ++i) {
    if (snap_compare(&fs->constants.vitems[i], &val) == 0) return i;
  }
  if (i > MAX_ARG) {
    parse_raise(snap, "snap_compile_error", "Too many constants");
  }
  *mvec_push(&fs->constants, SValue) = val;
  return i;
}

static void load_constant(Snap* snap, SnapFnState* fs, SValue val) {
  emit(fs, LOAD_CONSTANT, add_constant(snap, fs, val));
}

static void begin_scope(SnapFnState* fs) {
  fs->depth++;
}

static void end_scope(SnapFnState* fs) {
  bool captured = false;
  fs->depth--;
  while (fs->num_locals > 0 && fs->locals[fs->num_locals - 1].depth > fs->depth) {
    captured |= fs->locals[--fs->num_locals].captured;
  }
  // Leaving the scope frees its slots for reuse, so close captured ones
  if (captured) {
    emit(fs, CLOSE_UPVALS, fs->num_locals);
  }
}

// Collects the names that a set! in expr names (in quoted data too, which
// only makes locals of those names shared rather than copied)
static void scan_sets(SnapFnState* fs, SValue expr) {
  int i;
  if (is_cons(expr)) {
    SCons* curr = as_cons(expr);
    if (curr && is_form(curr->first) && as_form(curr->first) == TK_SET && is_cons(curr->rest) &&
        as_cons(curr->rest) && is_sym(as_cons(curr->rest)->first)) {
      if (fs->num_sets == fs->sets_capacity) {
        fs->sets_capacity = fs->sets_capacity ? 2 * fs->sets_capacity : 8;
        fs->sets = (SSym**)realloc(fs->sets, fs->sets_capacity * sizeof(SSym*));
      }
      fs->sets[fs->num_sets++] = as_sym(as_cons(curr->rest)->first);
    }
    for (; curr; curr = as_cons(curr->rest)) scan_sets(fs, curr->first);
  } else if (is_vec(expr)) {
    for (i = 0; i < as_vec(expr)->len; ++i) scan_sets(fs, *snap_vec_at(as_vec(expr), i));
  } else if (is_map(expr)) {
    for (i = 0; i < 2 * as_map(expr)->used; ++i) scan_sets(fs, as_map(expr)->entries[i]);
  }
}

static bool is_set(SnapFnState* fs, SSym* name) {
  int i;
  for (i = 0; i < fs->num_sets; ++i) {
    if (fs->sets[i] == name) return true;
  }
  return false;
}

// let and loop bindings can shadow a name bound earlier in the same form
static int add_local(Snap* snap, SnapFnState* fs, SSym* name, bool shadow) {
  int i;
  SnapLocal* local;
  for (i = fs->num_locals - 1; !shadow && i >= 0 && fs->locals[i].depth == fs->depth; --i) {
    if (fs->locals[i].name == name) {
      parse_raise(snap, "snap_compile_error", "Variable '%s' already defined in current scope", name->data);
    }
  }
  if (fs->num_locals >= MAX_LOCALS) {
    parse_raise(snap, "snap_compile_error", "Too many local variables");
  }
  local = &fs->locals[fs->num_locals++];
  local->name = name;
  local->depth = fs->depth;
  local->captured = false;
  local->copied = false;
  local->assigned = name->assigned || is_set(fs, name);
  local->alias = -1;
  local->constant = -1;
  if (fs->num_locals > fs->max_locals) {
    fs->max_locals = fs->num_locals;
  }
  return fs->num_locals - 1;
}

// The entry for a local name, or -1; inside an inlined function, only its
// own (see inline_global)
static int local_entry(SnapFnState* fs, SSym* name) {
  int i;
  for (i = fs->num_locals - 1; i >= fs->barrier; --i) {
    if (fs->locals[i].name == name) return i;
  }
  return -1;
}

// The slot of a local name, or -1 (for a name that's a constant too)
static int resolve_local(SnapFnState* fs, SSym* name) {
  int i = local_entry(fs, name);
  if (i < 0 || fs->locals[i].constant >= 0) return -1;
  return fs->locals[i].alias >= 0 ? fs->locals[i].alias : i;
}

// The constant a local name stands for, or -1
static int constant_local(SnapFnState* fs, SSym* name) {
  int i = local_entry(fs, name);
  return i >= 0 ? fs->locals[i].constant : -1;
}

// An inlined function's parameter that can't be used the way it was passed
// (see inline_global): compile again, with the name never passed that way
static void unalias(Snap* snap, SSym* name) {
  name->assigned = true;
  snap->compile_restart = true;
  longjmp(snap->jmp, 1);
}

static int add_upval(Snap* snap, SnapFnState* fs, int index, bool copied, bool islocal) {
  int i;
  uint32_t desc = ((uint32_t)index << 2) | copied << 1 | islocal;
  for (i = 0; i < fs->num_upvals; ++i) {
    if (fs->upvals[i] == desc) return i;
  }
  if (fs->num_upvals >= MAX_UPVALS) {
    parse_raise(snap, "snap_compile_error", "Too many closed over variables");
  }
  fs->upvals[fs->num_upvals] = desc;
  return fs->num_upvals++;
}

static int resolve_upval(Snap* snap, SnapFnState* fs, SSym* name) {
  int index;
  if (fs->top || fs->inlining) return -1;
  if (constant_local(fs->up, name) >= 0) unalias(snap, name); // Can't be closed over
  index = resolve_local(fs->up, name);
  if (index >= 0) {
    SnapLocal* local = &fs->up->locals[index];
    // A variable that's never assigned can be copied: no upvalue to make,
    // find or close
    if (local->assigned) local->captured = true;
    else local->copied = true;
    return add_upval(snap, fs, index, !local->assigned, true);
  }
  index = resolve_upval(snap, fs->up, name);
  if (index >= 0) {
    return add_upval(snap, fs, index, fs->up->upvals[index] & 2, false);
  }
  return -1;
}

// A constant global's value, as a constant of the function. These are
// matched by identity rather than contents, so that two constant vectors that
// happen to be equal stay two vectors.
static int add_global_constant(Snap* snap, SnapFnState* fs, SValue val) {
  int i;
  for (i = 0; i < fs->constants.vsize; ++i) {
    if (fs->constants.vitems[i].bits == val.bits) return i;
  }
  if (i > MAX_ARG) {
    parse_raise(snap, "snap_compile_error", "Too many constants");
  }
  *mvec_push(&fs->constants, SValue) = val;
  return i;
}

static void load_variable(Snap* snap, SnapFnState* fs, SSym* name) {
  int index;
  if ((index = constant_local(fs, name)) >= 0) {
    emit(fs, LOAD_CONSTANT, index);
  } else if ((index = resolve_local(fs, name)) >= 0) {
    emit(fs, LOAD_LOCAL, index);
  } else if ((index = resolve_upval(snap, fs, name)) >= 0) {
    emit(fs, (fs->upvals[index] & 2) ? LOAD_COPIED : LOAD_CLOSED, index);
  } else if (name->read_only) {
    // A constant (see set-const!): its value goes in the code in its place
    emit(fs, LOAD_CONSTANT, add_global_constant(snap, fs, name->value));
  } else {
    emit(fs, LOAD_GLOBAL, add_constant(snap, fs, create_obj(name)));
  }
}

static void store_global(Snap* snap, SnapFnState* fs, SSym* name) {
  if (name->read_only) {
    parse_raise(snap, "snap_compile_error", "%s is a constant", name->data);
  }
  emit(fs, STORE_GLOBAL, add_constant(snap, fs, create_obj(name)));
}

// A store to a local after its declaration, which scan_sets() found unless a
// macro made it. If an inner fn already copied the local, the forms are
// compiled again (see compile_forms()), with every local of this name
// assigned.
static void assign_local(Snap* snap, SnapLocal* local) {
  if (local->copied && !local->assigned) {
    local->name->assigned = true;
    snap->compile_restart = true;
    longjmp(snap->jmp, 1);
  }
  local->assigned = true;
}

// The local an upvalue of fs is in the end
static SnapLocal* upval_local(SnapFnState* fs, int index) {
  while (!(fs->upvals[index] & 1)) {
    index = fs->upvals[index] >> 2;
    fs = fs->up;
  }
  return &fs->up->locals[fs->upvals[index] >> 2];
}

static void store_variable(Snap* snap, SnapFnState* fs, SSym* name) {
  int index = local_entry(fs, name);
  // An inlined function's parameter that names the caller's local or a
  // constant can't be stored to
  if (index >= 0 && (fs->locals[index].alias >= 0 || fs->locals[index].constant >= 0)) unalias(snap, name);
  if ((index = resolve_local(fs, name)) >= 0) {
    assign_local(snap, &fs->locals[index]);
    emit(fs, STORE_LOCAL, index);
  } else if ((index = resolve_upval(snap, fs, name)) >= 0) {
    assign_local(snap, upval_local(fs, index));
    emit(fs, STORE_CLOSED, index);
  } else {
    store_global(snap, fs, name);
  }
}

// Conservative check for an inner fn (quoted data can give false positives)
// Whether expr uses a special form anywhere in it (like fn or set!)
static bool contains_form(SValue expr, int form) {
  int i;
  if (is_cons(expr)) {
    SCons* curr;
    for (curr = as_cons(expr); curr; curr = as_cons(curr->rest)) {
      if ((is_form(curr->first) && as_form(curr->first) == form) ||
          contains_form(curr->first, form)) {
        return true;
      }
    }
  } else if (is_vec(expr)) {
    for (i = 0; i < as_vec(expr)->len; ++i) {
      if (contains_form(*snap_vec_at(as_vec(expr), i), form)) return true;
    }
  } else if (is_map(expr)) {
    for (i = 0; i < 2 * as_map(expr)->used; ++i) {
      if (contains_form(as_map(expr)->entries[i], form)) return true;
    }
  }
  return false;
}

// How many values make up expr, counting up to limit
static int form_size(SValue expr, int limit) {
  int n = 1, i;
  if (is_cons(expr)) {
    SCons* curr;
    for (curr = as_cons(expr); curr && n < limit; curr = as_cons(curr->rest)) n += form_size(curr->first, limit - n);
  } else if (is_vec(expr)) {
    for (i = 0; i < as_vec(expr)->len && n < limit; ++i) n += form_size(*snap_vec_at(as_vec(expr), i), limit - n);
  } else if (is_map(expr)) {
    for (i = 0; i < 2 * as_map(expr)->used && n < limit; ++i) n += form_size(as_map(expr)->entries[i], limit - n);
  }
  return n;
}

// Conservative check for a use of a variable (shadowing gives false positives)
static bool references(SValue expr, SSym* name) {
  int i;
  if (is_sym(expr)) {
    return as_sym(expr) == name;
  } else if (is_cons(expr)) {
    SCons* curr;
    for (curr = as_cons(expr); curr; curr = as_cons(curr->rest)) {
      if (references(curr->first, name)) return true;
    }
  } else if (is_vec(expr)) {
    for (i = 0; i < as_vec(expr)->len; ++i) {
      if (references(*snap_vec_at(as_vec(expr), i), name)) return true;
    }
  } else if (is_map(expr)) {
    for (i = 0; i < 2 * as_map(expr)->used; ++i) {
      if (references(as_map(expr)->entries[i], name)) return true;
    }
  }
  return false;
}

// A quoted value is a single constant shared by every evaluation, so the
// vectors and maps in it can't be changed
static void make_read_only(SValue expr) {
  int i;
  if (is_cons(expr)) {
    SCons* curr;
    for (curr = as_cons(expr); curr; curr = as_cons(curr->rest)) make_read_only(curr->first);
  } else if (is_vec(expr)) {
    as_obj(expr)->read_only = true;
    for (i = 0; i < as_vec(expr)->len; ++i) make_read_only(*snap_vec_at(as_vec(expr), i));
  } else if (is_map(expr)) {
    as_obj(expr)->read_only = true;
    for (i = 0; i < 2 * as_map(expr)->used; ++i) make_read_only(as_map(expr)->entries[i]);
  }
}

static void compile(Snap* snap, SnapFnState* fs, SValue expr, bool tail);

// A local or literal as a binary op operand, emitting nothing; 0 otherwise
static int direct_operand(Snap* snap, SnapFnState* fs, SValue expr) {
  int index;
  switch (snap_type(expr)) {
    case STYPE_SYM:
      if ((index = constant_local(fs, as_sym(expr))) >= 0) return index + 1 <= MAX_OPERAND + 1 ? -(index + 1) : 0;
      index = resolve_local(fs, as_sym(expr));
      return index >= 0 && index + 1 <= MAX_OPERAND ? index + 1 : 0;
    case STYPE_INT: case STYPE_FLOAT: case STYPE_STR:
    case STYPE_BOOL: case STYPE_KEY: case STYPE_NIL:
      index = add_constant(snap, fs, expr);
      return index + 1 <= MAX_OPERAND + 1 ? -(index + 1) : 0;
    default:
      return 0;
  }
}

// Compiles (op a b). When b needs code of its own, a local or literal a is
// read by the op itself rather than pushed before b is evaluated.
static void compile_binary(Snap* snap, SnapFnState* fs, int op, SValue a, SValue b) {
  int left = direct_operand(snap, fs, a);
  if (left && !direct_operand(snap, fs, b)) {
    compile(snap, fs, b, false);
    emit_binary_operands(fs, op, left, take_operand(fs));
  } else {
    compile(snap, fs, a, false);
    compile(snap, fs, b, false);
    emit(fs, op, 0);
  }
}

static void compile_effect(Snap* snap, SnapFnState* fs, SValue expr);

// All but the last expression are only run for their effects
static void compile_body(Snap* snap, SnapFnState* fs, SCons* body, bool tail) {
  SCons* curr;
  for (curr = body; curr != NULL; curr = as_cons(curr->rest)) {
    if (is_empty(curr->rest)) {
      compile(snap, fs, curr->first, tail);
    } else {
      compile_effect(snap, fs, curr->first);
    }
  }
}

static bool is_pattern(SValue v);
static bool is_ignore(Snap* snap, SValue v);

// Special forms (if, or, not, + ...) are read as forms, not symbols, so they
// can't be bound. A map pattern can still take their key under another name.
static void check_bindable(Snap* snap, SValue name, bool in_map) {
  if (!is_form(name)) return;
  if (in_map) {
    parse_raise(snap, "snap_compile_error", "'%s' is a special form and can't be bound; use {x :%s} to rename it",
          form_name(as_form(name)), form_name(as_form(name)));
  }
  parse_raise(snap, "snap_compile_error", "'%s' is a special form and can't be bound", form_name(as_form(name)));
}

static void compile_def_pattern(Snap* snap, SnapFnState* fs, SValue pattern, SValue form);

static void compile_def(Snap* snap, SnapFnState* fs, SCons* args) {
  SSym* name;
  if (args) check_bindable(snap, args->first, false);
  if (list_length(args) != 2 || !(is_sym(args->first) || is_pattern(args->first))) {
    parse_raise(snap, "snap_compile_error", "Expected (def <id or pattern> <expr>)");
  }
  if (!is_sym(args->first) || is_ignore(snap, args->first)) {
    compile_def_pattern(snap, fs, args->first, list_nth(args, 1));
    emit(fs, LOAD_NIL, 0);
    return;
  }
  name = as_sym(args->first);
  compile(snap, fs, list_nth(args, 1), false);
  if (fs->depth > 0) {
    emit(fs, STORE_LOCAL, add_local(snap, fs, name, false));
  } else {
    store_global(snap, fs, name);
  }
  emit(fs, LOAD_NIL, 0); /* TODO: Hack */
}

static void compile_fn(Snap* snap, SnapFnState* up, SCons* args);

// (defmacro name [params] body...) compiles like (def name (fn [params]
// body...)), with the function marked as a macro
static void compile_defmacro(Snap* snap, SnapFnState* fs, SCons* args) {
  if (!args || !is_sym(args->first)) {
    parse_raise(snap, "snap_compile_error", "Expected (defmacro <id> [<params>] <expr>...)");
  }
  if (fs->depth > 0 || !fs->top) {
    parse_raise(snap, "snap_compile_error", "defmacro must be at the top level");
  }
  compile_fn(snap, fs, as_cons(args->rest));
  load_constant(snap, fs, create_cfunc(builtin_make_macro));
  emit(fs, CALL, 1);
  store_global(snap, fs, as_sym(args->first));
  emit(fs, LOAD_NIL, 0); /* TODO: Hack */
}

// The macro that (name args...) calls: the value of the global `name`, if
// it's a macro and no local or closed over variable has the name
static bool find_macro(SnapFnState* fs, SValue name, SValue* macro) {
  SValue val;
  for (; fs; fs = fs->up) {
    if (local_entry(fs, as_sym(name)) >= 0) return false;
    if (fs->top || fs->inlining) break;
  }
  val = as_sym(name)->value;
  if (!is_obj(val) || !as_obj(val)->macro) return false;
  *macro = val;
  return true;
}

// Calls a macro with the forms it was given, unevaluated, and returns the
// form it makes: an error if it panics or returns one
SValue macro_expand(Snap* snap, SValue macro, SCons* args) {
  int i, n = list_length(args);
  SValue* argv = (SValue*)malloc((n + 1) * sizeof(SValue));
  SValue form;
  for (i = 0; i < n; ++i, args = as_cons(args->rest)) argv[i] = args->first;
  form = snap_call(snap, macro, argv, n); // The arguments are rooted by the call
  free(argv);
  return form;
}

static void compile_set(Snap* snap, SnapFnState* fs, SCons* args) {
  if (list_length(args) != 2 || !is_sym(args->first)) {
    parse_raise(snap, "snap_compile_error", "Expected (set! <id> <expr>)");
  }
  compile(snap, fs, list_nth(args, 1), false);
  store_variable(snap, fs, as_sym(args->first));
  emit(fs, LOAD_NIL, 0); /* TODO: Hack */
}

// Jumps to the same place are chained through their args until it's known:
// each holds the index of the one before, or -1
static int chain_jump(SnapFnState* fs, int op, int chain) {
  int at = emit_jump(fs, op);
  fs->insts.vitems[at] = INST(op, chain);
  return at;
}

static void patch_jumps(Snap* snap, SnapFnState* fs, int chain) {
  while (chain >= 0) {
    int prev = INST_ARG(fs->insts.vitems[chain]);
    patch_jump(snap, fs, chain);
    chain = prev;
  }
}

static void compile_compare(Snap* snap, SnapFnState* fs, int opcode, SCons* args);

// Compiles a condition that jumps when its truth is `when`, and otherwise
// falls through, leaving nothing on the stack. Returns the chain of its
// jumps, added to `chain`. An and (or) jumps out at the first false (true)
// operand, so each compare branches by itself instead of pushing a bool. A
// condition that turns out to be a constant, like cond's last true, needs no
// test.
static int compile_jump(Snap* snap, SnapFnState* fs, SValue cond, bool when, int chain) {
  SCons* sexpr = is_cons(cond) && !is_empty(cond) ? as_cons(cond) : NULL;
  int form = sexpr && is_form(sexpr->first) ? as_form(sexpr->first) : -1;
  SCons* args = sexpr ? as_cons(sexpr->rest) : NULL;
  parse_enter(snap, "snap_compile_error");
  if ((form == TK_AND || form == TK_OR) && args) {
    // Every operand but the last jumps on false for and (true for or): out
    // of the condition when that's `when`, otherwise past it
    bool stop = form == TK_OR;
    int past = -1;
    for (; !is_empty(args->rest); args = as_cons(args->rest)) {
      if (stop == when) chain = compile_jump(snap, fs, args->first, stop, chain);
      else past = compile_jump(snap, fs, args->first, stop, past);
    }
    chain = compile_jump(snap, fs, args->first, when, chain);
    patch_jumps(snap, fs, past);
  } else if (form == TK_NOT && list_length(args) == 1) {
    chain = compile_jump(snap, fs, args->first, !when, chain);
  } else {
    // (not= ...) is (= ...) with the jump the other way
    if (form == TK_NE) {
      compile_compare(snap, fs, EQUALS, args);
      when = !when;
    } else {
      compile(snap, fs, cond, false);
    }
    if (fs->last_target != pc(fs) && (last_op(fs) == LOAD_CONSTANT || last_op(fs) == LOAD_NIL)) {
      SnapInst inst = fs->insts.vitems[pc(fs) - 1];
      bool truth = INST_OP(inst) == LOAD_CONSTANT && !is_falsy(fs->constants.vitems[INST_ARG(inst)]);
      fs->insts.vsize--;
      adjust_stack(fs, -1);
      if (truth == when) chain = chain_jump(fs, JUMP, chain);
    } else {
      chain = chain_jump(fs, when ? JUMP_TRUE : JUMP_FALSE, chain);
    }
  }
  snap->c_depth--;
  return chain;
}

// Code that can't run, like the else of a condition that's always true, is
// still compiled (for its errors), then dropped
static void compile_dead(Snap* snap, SnapFnState* fs, SValue expr, bool tail, bool effect) {
  int at = pc(fs), target = fs->last_target, stack = fs->stack;
  if (effect) compile_effect(snap, fs, expr);
  else compile(snap, fs, expr, tail);
  fs->insts.vsize = at;
  fs->last_target = target;
  fs->stack = stack;
}

// (if cond then else), for its value or (with `effect`) only its effects,
// where a missing else (nil) needs no code
static void compile_branches(Snap* snap, SnapFnState* fs, SCons* args, bool tail, bool effect) {
  int cond_jumps, jump, stack;
  SValue then = list_nth(args, 1), other = list_nth(args, 2);
  cond_jumps = compile_jump(snap, fs, args->first, false, -1);
  stack = fs->stack;
  if (effect) compile_effect(snap, fs, then);
  else compile(snap, fs, then, tail);
  if (cond_jumps < 0) { // Always true
    compile_dead(snap, fs, other, tail, effect);
    return;
  }
  if (effect && is_nil(other)) {
    patch_jumps(snap, fs, cond_jumps);
    return;
  }
  jump = emit_jump(fs, JUMP);
  patch_jumps(snap, fs, cond_jumps);
  fs->stack = stack;
  if (effect) compile_effect(snap, fs, other);
  else compile(snap, fs, other, tail);
  patch_jump(snap, fs, jump);
}

static void compile_if(Snap* snap, SnapFnState* fs, SCons* args, bool tail) {
  if (list_length(args) != 3) {
    parse_raise(snap, "snap_compile_error", "Expected (if <cond> <then> <else>)");
  }
  compile_branches(snap, fs, args, tail, false);
}

/* Destructuring. A pattern in place of a name in let, loop or fn binds the
 * names in it to parts of the value:
 *   [a b & more :as all]  (nth v 0), (nth v 1), (nthnext v 2) and v itself
 *   {:keys [a b] :as m}   (get m :a), (get m :b) and m itself
 * Patterns nest. The compiler rewrites them into plain bindings of those
 * calls before compiling, so they run as fast as the calls written out (and
 * a vector too short for its pattern is nth's error). */

static bool is_pattern(SValue v) { return is_vec(v) || is_map(v); }

static SValue new_gensym(Snap* snap);

// _ in a binding binds nothing: in a pattern its position isn't read, and as
// a whole binding (let, loop, fn) the value goes in a name nothing can read
static bool is_ignore(Snap* snap, SValue v) { return is_sym(v) && as_sym(v) == snap_sym_new(snap, "_"); }

static SSym* binding_name(Snap* snap, SValue name) {
  return is_ignore(snap, name) ? as_sym(new_gensym(snap)) : as_sym(name);
}

static bool is_key_named(Snap* snap, SValue v, const char* name) {
  return is_obj(v) && as_obj(v) == (SObject*)snap_key_new(snap, name);
}

// Whether a pattern (or a name) binds name
static bool binds(Snap* snap, SValue pattern, SSym* name) {
  int i;
  if (is_sym(pattern)) return as_sym(pattern) == name;
  if (is_vec(pattern)) {
    for (i = 0; i < as_vec(pattern)->len; ++i) {
      if (binds(snap, *snap_vec_at(as_vec(pattern), i), name)) return true;
    }
  } else if (is_map(pattern)) {
    // {:keys [names] :as name :or {defaults} target key...}
    SMap* m = as_map(pattern);
    for (i = 0; i < m->used; ++i) {
      SValue k = m->entries[2 * i], v = m->entries[2 * i + 1];
      if (k.bits == SV_DELETED || is_key_named(snap, k, "or")) continue;
      if (is_key_named(snap, k, "keys") || is_key_named(snap, k, "as") ? binds(snap, v, name) : binds(snap, k, name)) {
        return true;
      }
    }
  }
  return false;
}

// Set while rewriting bindings that themselves bind nth, get or nthnext
static bool rewrite_hides_globals;

static void check_hidden_globals(Snap* snap, SVec* names, int step) {
  static const char* globals[] = { "nth", "get", "nthnext" };
  int i, j;
  rewrite_hides_globals = false;
  for (i = 0; i < names->len; i += step) {
    for (j = 0; j < 3; ++j) {
      if (binds(snap, *snap_vec_at(names, i), snap_sym_new(snap, globals[j]))) rewrite_hides_globals = true;
    }
  }
}

// A global function, for the rewritten calls: its name, so the call gets the
// instruction for it, unless a local of that name hides it
static SValue global_fn(SnapFnState* fs, const char* name, SCFunc fn, Snap* snap) {
  SSym* sym = snap_sym_new(snap, name);
  if (rewrite_hides_globals) return create_cfunc(fn);
  for (; fs; fs = fs->up) {
    if (local_entry(fs, sym) >= 0) return create_cfunc(fn);
  }
  return create_obj(sym);
}

// (f a b), with a and b already rooted (symbols, keywords and ints are)
static SValue call2(Snap* snap, SValue f, SValue a, SValue b) {
  SnapList list = { NULL, NULL };
  *list_add(snap, &list) = f;
  *list_add(snap, &list) = a;
  *list_add(snap, &list) = b;
  return list_done(snap, &list);
}

static SValue new_gensym(Snap* snap) {
  SValue sym;
  builtin_gensym(snap, NULL, 0, &sym); // Interned, so it lives forever
  return sym;
}

// (if (= name nil) default name), for a map pattern's :or
static SValue or_default(Snap* snap, SValue name, SValue dflt) {
  SnapList test = { NULL, NULL }, form = { NULL, NULL };
  SValue t;
  *list_add(snap, &test) = create_form(TK_EQ);
  *list_add(snap, &test) = name;
  *list_add(snap, &test) = create_nil();
  t = list_done(snap, &test);
  anchor(snap, as_obj(t));
  *list_add(snap, &form) = create_form(TK_IF);
  *list_add(snap, &form) = t;
  *list_add(snap, &form) = dflt; // Rooted by the pattern
  *list_add(snap, &form) = name;
  t = list_done(snap, &form);
  release(snap);
  return t;
}

static void push_binding(Snap* snap, SVec* out, SValue name, SValue form) {
  if (is_obj(form)) anchor(snap, as_obj(form));
  vec_push(snap, out, name);
  vec_push(snap, out, form);
  if (is_obj(form)) release(snap);
}

static bool find_macro(SnapFnState* fs, SValue name, SValue* macro);
static void destructure(Snap* snap, SnapFnState* fs, SValue pattern, SValue form, SVec* out);

// (#unpack a b ...), the name of a binding to the first parts of a call's
// value, which let and def take with UNPACK: no vector is made when the call
// returns a literal ([x z] at the end of a function)
static SSym* unpack_sym(Snap* snap) { return snap_sym_new(snap, "#unpack"); }

static bool is_unpack(Snap* snap, SValue name) {
  return is_cons(name) && !is_empty(name) && is_sym(as_cons(name)->first) && as_sym(as_cons(name)->first) == unpack_sym(snap);
}

// [x [y z] _ w] from a call: an unpack binding of the parts read, then their
// nested patterns. Not with & or :as (which need the whole value), nor once
// nth isn't the built-in (as the parts are taken with it).
static bool unpack_binding(Snap* snap, SnapFnState* fs, SVec* v, SValue form, SVec* out) {
  SValue head, macro, names;
  SVec* nested;
  SnapList list = { NULL, NULL };
  int i, n = 0;
  if (!is_cons(form) || is_empty(form)) return false;
  head = as_cons(form)->first;
  if (!(is_cons(head) || (is_sym(head) && !find_macro(fs, head, &macro)))) return false;
  if (snap_sym_new(snap, "nth")->value.bits != create_cfunc(builtin_nth).bits) return false;
  for (i = 0; i < v->len; ++i) {
    SValue p = *snap_vec_at(v, i);
    if (is_sym(p) && as_sym(p) == snap_sym_new(snap, "&")) return false;
    if (is_obj(p) && as_obj(p) == (SObject*)snap_key_new(snap, "as")) return false;
    if (!is_ignore(snap, p)) n = i + 1;
  }
  if (n == 0 || n > 255) return false;
  nested = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, 0));
  *list_add(snap, &list) = create_obj(unpack_sym(snap));
  for (i = 0; i < n; ++i) {
    SValue p = *snap_vec_at(v, i);
    if (is_sym(p) && !is_ignore(snap, p)) {
      *list_add(snap, &list) = p;
    } else {
      SValue g = new_gensym(snap);
      *list_add(snap, &list) = g;
      if (!is_ignore(snap, p)) {
        vec_push(snap, nested, p);
        vec_push(snap, nested, g);
      }
    }
  }
  names = list_done(snap, &list);
  push_binding(snap, out, names, form);
  for (i = 0; i < nested->len; i += 2) {
    destructure(snap, fs, *snap_vec_at(nested, i), *snap_vec_at(nested, i + 1), out);
  }
  release(snap);
  return true;
}

// Takes the parts of the value on the stack: n values in its place
static void emit_unpack(SnapFnState* fs, int n) {
  emit(fs, UNPACK, n);
  adjust_stack(fs, n); // Room for the parts above the value, while it's unpacked
  adjust_stack(fs, -1);
}

// Appends the plain bindings (name, form, ...) for binding pattern to the
// value of form, to out (which the caller roots)
static void destructure(Snap* snap, SnapFnState* fs, SValue pattern, SValue form, SVec* out) {
  SValue tmp;
  int i, n;
  if (is_sym(pattern)) {
    push_binding(snap, out, pattern, form);
    return;
  }
  check_bindable(snap, pattern, false);
  if (!is_pattern(pattern)) {
    parse_raise(snap, "snap_compile_error", "Expected a name, a [pattern] or a {:keys [names]} pattern");
  }
  if (is_vec(pattern) && unpack_binding(snap, fs, as_vec(pattern), form, out)) return;
  // The whole value, once, in a name the parts are taken from
  tmp = is_sym(form) ? form : new_gensym(snap);
  if (!is_sym(form)) push_binding(snap, out, tmp, form);
  if (is_vec(pattern)) {
    SVec* v = as_vec(pattern);
    SSym* amp = snap_sym_new(snap, "&");
    SKeyword* as = snap_key_new(snap, "as");
    for (i = 0, n = 0; i < v->len; ++i) {
      SValue p = *snap_vec_at(v, i);
      if (is_sym(p) && as_sym(p) == amp) {
        if (i + 1 >= v->len) parse_raise(snap, "snap_compile_error", "Expected a pattern after &");
        if (!is_ignore(snap, *snap_vec_at(v, ++i))) {
          destructure(snap, fs, *snap_vec_at(v, i),
                      call2(snap, global_fn(fs, "nthnext", builtin_nthnext, snap), tmp, create_int(n)), out);
        }
        if (i + 1 < v->len && !(is_obj(*snap_vec_at(v, i + 1)) && as_obj(*snap_vec_at(v, i + 1)) == (SObject*)as)) {
          parse_raise(snap, "snap_compile_error", "Expected only :as after & <pattern>");
        }
      } else if (is_obj(p) && as_obj(p) == (SObject*)as) {
        if (i + 1 < v->len) check_bindable(snap, *snap_vec_at(v, i + 1), false);
        if (i + 2 != v->len || !is_sym(*snap_vec_at(v, i + 1))) {
          parse_raise(snap, "snap_compile_error", "Expected :as <name> at the end of the pattern");
        }
        if (!is_ignore(snap, *snap_vec_at(v, ++i))) push_binding(snap, out, *snap_vec_at(v, i), tmp);
      } else if (is_ignore(snap, p)) {
        n++; // Skipped, not read
      } else {
        destructure(snap, fs, p, call2(snap, global_fn(fs, "nth", builtin_nth, snap), tmp, create_int(n++)), out);
      }
    }
  } else {
    // {:keys [a b] c :k [d e] :v :or {a 1 c 2} :as m}: a = (get m :a),
    // c = (get m :k), [d e] from (get m :v), and a and c, when nil, their
    // defaults (evaluated only then)
    SMap* m = as_map(pattern);
    SValue* keys = map_get(m, create_obj(snap_key_new(snap, "keys")));
    SValue* as = map_get(m, create_obj(snap_key_new(snap, "as")));
    SValue* ors = map_get(m, create_obj(snap_key_new(snap, "or")));
    SMap* dflts = NULL;
    if (keys && !is_vec(*keys)) parse_raise(snap, "snap_compile_error", "Expected :keys [names]");
    if (as) check_bindable(snap, *as, false);
    if (as && !is_sym(*as)) parse_raise(snap, "snap_compile_error", "Expected :as <name>");
    if (ors) {
      if (!is_map(*ors)) parse_raise(snap, "snap_compile_error", "Expected :or {name default ...}");
      dflts = as_map(*ors);
    }
    if (keys) {
      for (i = 0; i < as_vec(*keys)->len; ++i) {
        SValue name = *snap_vec_at(as_vec(*keys), i);
        SValue* d;
        check_bindable(snap, name, true);
        if (!is_sym(name) || is_ignore(snap, name)) parse_raise(snap, "snap_compile_error", "Expected :keys [names]");
        push_binding(snap, out, name,
                     call2(snap, global_fn(fs, "get", builtin_get, snap), tmp,
                           create_obj(snap_key_new(snap, as_sym(name)->data))));
        if (dflts && (d = map_get(dflts, name))) push_binding(snap, out, name, or_default(snap, name, *d));
      }
    }
    for (i = 0; i < m->used; ++i) {
      SValue target = m->entries[2 * i], key = m->entries[2 * i + 1], get;
      if (target.bits == SV_DELETED || is_key_named(snap, target, "keys") || is_key_named(snap, target, "as") ||
          is_key_named(snap, target, "or")) {
        continue;
      }
      check_bindable(snap, target, false);
      if (!is_sym(target) && !is_pattern(target)) {
        parse_raise(snap, "snap_compile_error", "Expected :keys, :as, :or or <name or pattern> <key> in a map pattern");
      }
      if (!is_key(key) && !is_str(key) && !is_int(key)) {
        parse_raise(snap, "snap_compile_error", "Expected a keyword, string or int to look up after a name in a map pattern");
      }
      if (is_ignore(snap, target)) continue;
      get = call2(snap, global_fn(fs, "get", builtin_get, snap), tmp, key);
      if (is_sym(target)) {
        SValue* d;
        push_binding(snap, out, target, get);
        if (dflts && (d = map_get(dflts, target))) push_binding(snap, out, target, or_default(snap, target, *d));
      } else {
        anchor(snap, as_obj(get));
        destructure(snap, fs, target, get, out);
        release(snap);
      }
    }
    if (dflts) {
      // Every default is for a name bound straight from a key here
      for (i = 0; i < dflts->used; ++i) {
        SValue name = dflts->entries[2 * i];
        bool found = false;
        int j;
        if (name.bits == SV_DELETED) continue;
        if (!is_sym(name) || is_ignore(snap, name)) parse_raise(snap, "snap_compile_error", "Expected :or {name default ...}");
        for (j = 0; keys && j < as_vec(*keys)->len && !found; ++j) found = as_sym(*snap_vec_at(as_vec(*keys), j)) == as_sym(name);
        for (j = 0; j < m->used && !found; ++j) found = m->entries[2 * j].bits == name.bits;
        if (!found) parse_raise(snap, "snap_compile_error", "'%s' in :or isn't a name this pattern binds from a key", as_sym(name)->data);
      }
    }
    if (as && !is_ignore(snap, *as)) push_binding(snap, out, *as, tmp);
  }
}

// (head first body...), rooted by the caller once made
static SValue make_form(Snap* snap, SValue head, SValue first, SValue body) {
  SCons* c = (SCons*)anchor(snap, (SObject*)cons_new(snap));
  SCons* d = cons_new(snap);
  c->first = head;
  c->rest = create_obj(d);
  d->first = first;
  d->rest = body;
  release(snap);
  return create_obj(c);
}

static void compile_fn(Snap* snap, SnapFnState* up, SCons* args) {
  int i;
  SSym* amp;
  SVec* params;
  SnapFnState* fs;
  SCode* code;

  if (!args || !is_vec(args->first)) {
    parse_raise(snap, "snap_compile_error", "Expected parameter list");
  }
  if (!as_cons(args->rest)) {
    parse_raise(snap, "snap_compile_error", "Expected expressions");
  }

  params = as_vec(args->first);
  // Patterns: (fn [[x y] r] body...) is (fn [#p r] (let [[x y] #p] body...))
  for (i = 0; i < params->len && !is_pattern(*snap_vec_at(params, i)); ++i) {}
  if (i < params->len) {
    SVec* names;
    check_hidden_globals(snap, params, 1);
    names = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, params->len));
    SVec* pats = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, 0));
    SValue body, form;
    SCons* fargs;
    for (i = 0; i < params->len; ++i) {
      SValue p = *snap_vec_at(params, i);
      if (is_pattern(p)) {
        SValue g = new_gensym(snap);
        vec_push(snap, names, g);
        vec_push(snap, pats, p);
        vec_push(snap, pats, g);
      } else {
        vec_push(snap, names, p);
      }
    }
    body = make_form(snap, create_form(TK_LET), create_obj(pats), args->rest);
    anchor(snap, as_obj(body));
    fargs = cons_new(snap);
    fargs->first = create_obj(names);
    form = create_obj(cons_new(snap));
    as_cons(form)->first = body;
    fargs->rest = form;
    anchor(snap, (SObject*)fargs);
    compile_fn(snap, up, fargs);
    release(snap);
    release(snap);
    release(snap);
    release(snap);
    return;
  }
  fs = fn_state_new(snap);
  fs->depth = 1; // def inside a fn makes locals
  scan_sets(fs, args->rest);

  // [a b & more] puts the arguments after the first two in a list, `more`
  amp = snap_sym_new(snap, "&");
  for (i = 0; i < params->len; ++i) {
    SValue param = *snap_vec_at(params, i);
    check_bindable(snap, param, false);
    if (!is_sym(param)) {
      parse_raise(snap, "snap_compile_error", "Expected symbol");
    }
    if (as_sym(param) == amp) {
      if (i != params->len - 2) {
        parse_raise(snap, "snap_compile_error", "Expected one parameter after &");
      }
      fs->variadic = true;
      continue;
    }
    add_local(snap, fs, binding_name(snap, param), false);
  }
  fs->num_args = fs->num_locals;

  fs->fn_loop.top = 0;
  fs->fn_loop.base = 0;
  fs->fn_loop.count = fs->num_args;
  fs->fn_loop.closes = contains_form(args->rest, TK_FN);
  fs->loop = &fs->fn_loop;

  compile_body(snap, fs, as_cons(args->rest), true);
  emit(fs, RETURN, 0);

  // A small function keeps its source, for calls to it to inline (see
  // inline_global). What it closes over or defines, a recur to the function
  // itself, or a try (which returns from it), can't be inlined.
  if (!fs->variadic && fs->num_upvals == 0 && params->len <= MAX_INLINE_ARGS &&
      form_size(args->rest, INLINE_SIZE + 1) <= INLINE_SIZE && !contains_form(args->rest, TK_FN) &&
      !contains_form(args->rest, TK_RECUR) && !contains_form(args->rest, TK_DEF) &&
      !contains_form(args->rest, TK_DEFMACRO) && !contains_form(args->rest, TK_TRY)) {
    fs->inline_form = add_global_constant(snap, fs, create_obj(args));
  }

  code = code_new(snap, fs);
  fn_state_free(snap);
  load_constant(snap, up, create_obj(code)); // Rooted again as a constant
  if (code->num_upvals > 0) {
    emit(up, CLOSURE, 0);
  }
}

static void compile_let(Snap* snap, SnapFnState* fs, SCons* args, bool loop, bool tail);

// A let or loop with patterns among its names. A let becomes plain bindings:
// (let [[x y] p] ...) is (let [x (nth p 0) y (nth p 1)] ...). A loop needs a
// slot for each of its bindings, for recur, so
//   (loop [[x y] p i 0] body...)
// is
//   (let [#t p [x y] #t i 0] (loop [#s #t i i] (let [[x y] #s] body...)))
// where the outer let lets later values use earlier names, as in any loop.
static void compile_let_patterns(Snap* snap, SnapFnState* fs, SCons* args, bool loop, bool tail) {
  SVec* bindings = as_vec(args->first);
  SVec* outer = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, 0));
  SValue form;
  int i;
  check_hidden_globals(snap, bindings, 2);
  if (!loop) {
    for (i = 0; i < bindings->len; i += 2) {
      destructure(snap, fs, *snap_vec_at(bindings, i), *snap_vec_at(bindings, i + 1), outer);
    }
    form = make_form(snap, create_form(TK_LET), create_obj(outer), args->rest);
  } else {
    SVec* slots = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, 0));
    SVec* pats = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, 0));
    SValue body, inner;
    for (i = 0; i < bindings->len; i += 2) {
      SValue name = *snap_vec_at(bindings, i);
      if (is_pattern(name)) {
        SValue t = new_gensym(snap), s = new_gensym(snap);
        push_binding(snap, outer, t, *snap_vec_at(bindings, i + 1));
        destructure(snap, fs, name, t, outer);
        vec_push(snap, slots, s);
        vec_push(snap, slots, t);
        vec_push(snap, pats, name);
        vec_push(snap, pats, s);
      } else {
        push_binding(snap, outer, name, *snap_vec_at(bindings, i + 1));
        vec_push(snap, slots, name);
        vec_push(snap, slots, name);
      }
    }
    body = make_form(snap, create_form(TK_LET), create_obj(pats), args->rest);
    anchor(snap, as_obj(body));
    inner = create_obj(cons_new(snap));
    as_cons(inner)->first = body;
    inner = make_form(snap, create_form(TK_LOOP), create_obj(slots), inner);
    anchor(snap, as_obj(inner));
    form = create_obj(cons_new(snap));
    as_cons(form)->first = inner;
    form = make_form(snap, create_form(TK_LET), create_obj(outer), form);
    release(snap);
    release(snap);
    release(snap);
    release(snap);
    anchor(snap, as_obj(form)); // In place of outer, which it holds
    compile(snap, fs, form, tail);
    release(snap);
    return;
  }
  anchor(snap, as_obj(form));
  compile(snap, fs, form, tail);
  release(snap);
  release(snap);
}

// (def [x z] (place)) defines each name the pattern binds, as plain defs:
//   (def #t (place)) (def x (nth #t 0)) (def z (nth #t 1))
// (not a let around defs, where def makes locals). At the top level the
// hidden names are globals, set back to nil after so they don't hold the
// value. (def _ expr) runs expr and defines nothing.
static void compile_def_pattern(Snap* snap, SnapFnState* fs, SValue pattern, SValue form) {
  SVec* out;
  SVec* names;
  int i, j, slots[256];
  if (is_ignore(snap, pattern)) {
    compile_effect(snap, fs, form);
    return;
  }
  out = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, 1));
  vec_push(snap, out, pattern);
  check_hidden_globals(snap, out, 1);
  out->len = 0;
  destructure(snap, fs, pattern, form, out);
  // Every name defined, in order, with an unpack binding's names spread out
  names = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, 0));
  for (i = 0; i < out->len; i += 2) {
    SValue name = *snap_vec_at(out, i);
    int n = 1, k;
    compile(snap, fs, *snap_vec_at(out, i + 1), false);
    if (is_unpack(snap, name)) {
      SCons* c;
      n = list_length(as_cons(as_cons(name)->rest));
      emit_unpack(fs, n);
      for (c = as_cons(as_cons(name)->rest); c; c = as_cons(c->rest)) vec_push(snap, names, c->first);
    } else {
      vec_push(snap, names, name);
    }
    // Each name's slot (inside a function: a name bound twice here, as a
    // default after its get, keeps its first), then stored from the top of
    // the stack, the last part first
    for (k = names->len - n; k < names->len; ++k) {
      SSym* sym = as_sym(*snap_vec_at(names, k));
      if (fs->depth == 0) continue;
      for (j = 0; j < k && as_sym(*snap_vec_at(names, j)) != sym; ++j) {}
      if (j < k) {
        slots[k - (names->len - n)] = resolve_local(fs, sym);
        assign_local(snap, &fs->locals[slots[k - (names->len - n)]]); // Its default
      } else {
        slots[k - (names->len - n)] = add_local(snap, fs, sym, false);
      }
    }
    for (k = names->len - 1; k >= names->len - n; --k) {
      if (fs->depth > 0) {
        emit(fs, STORE_LOCAL, slots[k - (names->len - n)]);
      } else {
        store_global(snap, fs, as_sym(*snap_vec_at(names, k)));
      }
    }
  }
  if (fs->depth == 0) {
    for (i = 0; i < names->len; ++i) {
      SSym* name = as_sym(*snap_vec_at(names, i));
      if (!binds(snap, pattern, name)) {
        emit(fs, LOAD_NIL, 0);
        store_global(snap, fs, name);
      }
    }
  }
  release(snap);
  release(snap);
}

// Bindings alternate names and values: (let [a 1 b 2] ...)
static void compile_let(Snap* snap, SnapFnState* fs, SCons* args, bool loop, bool tail) {
  int i, base, count, extra;
  SVec* bindings;
  SnapLoop recur_target;

  if (!args || !is_vec(args->first)) {
    parse_raise(snap, "snap_compile_error", "Expected binding vector");
  }
  if (!as_cons(args->rest)) {
    parse_raise(snap, "snap_compile_error", "Expected expressions");
  }

  bindings = as_vec(args->first);
  count = bindings->len / 2;
  if (bindings->len % 2 != 0) {
    parse_raise(snap, "snap_compile_error", "Expected bindings [<id> <expr> ...]");
  }
  for (i = 0; i < count && !is_pattern(*snap_vec_at(bindings, 2 * i)); ++i) {}
  if (i < count) {
    compile_let_patterns(snap, fs, args, loop, tail);
    return;
  }
  for (i = 0; i < count; ++i) {
    check_bindable(snap, *snap_vec_at(bindings, 2 * i), false);
    if (!loop && is_unpack(snap, *snap_vec_at(bindings, 2 * i))) continue; // From a pattern
    if (!is_sym(*snap_vec_at(bindings, 2 * i))) {
      parse_raise(snap, "snap_compile_error", "Expected bindings [<id> <expr> ...]");
    }
  }

  // In order, so each value can use the names before it. Scopes inside a
  // value free their slots, so the bindings get consecutive slots (which
  // recur relies on).
  begin_scope(fs);
  base = fs->num_locals;
  for (i = 0, extra = 0; i < count; ++i) {
    SValue name = *snap_vec_at(bindings, 2 * i);
    compile(snap, fs, *snap_vec_at(bindings, 2 * i + 1), false);
    if (is_unpack(snap, name)) {
      // The parts in consecutive slots, stored from the top of the stack
      int n = list_length(as_cons(as_cons(name)->rest)), first = fs->num_locals, j;
      SCons* c;
      emit_unpack(fs, n);
      for (c = as_cons(as_cons(name)->rest); c; c = as_cons(c->rest)) add_local(snap, fs, as_sym(c->first), true);
      for (j = n - 1; j >= 0; --j) emit(fs, STORE_LOCAL, first + j);
      extra += n - 1;
    } else {
      emit(fs, STORE_LOCAL, add_local(snap, fs, binding_name(snap, name), true));
    }
  }
  assert(fs->num_locals == base + count + extra);

  if (loop) {
    recur_target.top = pc(fs);
    recur_target.base = base;
    recur_target.count = count;
    recur_target.closes = contains_form(args->rest, TK_FN);
    recur_target.up = fs->loop;
    fs->loop = &recur_target;
  }

  // A loop body is a fresh tail context for its recur
  compile_body(snap, fs, as_cons(args->rest), loop || tail);

  if (loop) {
    fs->loop = recur_target.up;
  }
  end_scope(fs);
}

// An argument that is just the target's own variable needs no store
static bool is_same_local(SnapFnState* fs, SValue arg, int slot) {
  return is_sym(arg) && resolve_local(fs, as_sym(arg)) == slot;
}

// A counting loop, where the loop starts with a compare of one of its
// variables, (if (< i limit) ...), and recur steps that variable by a
// constant, (+ i 1), can end in a single STEP instruction: it adds the
// constant and runs the compare, and jumps straight into the body when it
// holds. Returns the STEP instruction, or -1 (with *var unchanged) when the
// loop doesn't qualify.
static int step_inst(Snap* snap, SnapFnState* fs, SnapLoop* target, SCons* args, int* var) {
  static const int steps[] = { STEP_LT, STEP_LE, STEP_GT, STEP_GE };
  SnapInst header;
  SValue arg, k, v;
  SCons* curr, * operands;
  int slot, op, step, i;

  if (target->closes || target->top + 1 >= pc(fs)) return -1;
  header = fs->insts.vitems[target->top];
  op = INST_OP(header);
  if (op < LESS_THAN || op > GREATER_EQUALS || OPERAND_RIGHT(header) == 0 ||
      INST_OP(fs->insts.vitems[target->top + 1]) != JUMP_FALSE) {
    return -1;
  }
  slot = OPERAND_LEFT(header) - 1;
  if (slot < target->base || slot >= target->base + target->count || slot + 1 > MAX_DEST) return -1;

  // The variable's new value is (+ i k), (+ k i) or (- i k)
  arg = list_nth(args, slot - target->base);
  if (!is_cons(arg) || !is_form(as_cons(arg)->first)) return -1;
  operands = as_cons(as_cons(arg)->rest);
  if (list_length(operands) != 2) return -1;
  if (as_form(as_cons(arg)->first) == TK_ADD && is_int(operands->first)) {
    k = operands->first;
    v = list_nth(operands, 1);
  } else if (as_form(as_cons(arg)->first) == TK_ADD || as_form(as_cons(arg)->first) == TK_SUB) {
    v = operands->first;
    k = list_nth(operands, 1);
  } else {
    return -1;
  }
  if (!is_int(k) || !is_sym(v) || resolve_local(fs, as_sym(v)) != slot) return -1;
  if (as_form(as_cons(arg)->first) == TK_SUB) k = create_int(-as_int(k));

  // Nothing else may assign the variable before it's stepped
  for (i = 0, curr = args; curr; ++i, curr = as_cons(curr->rest)) {
    if (i != slot - target->base && contains_form(curr->first, TK_SET)) return -1;
  }

  step = add_constant(snap, fs, k);
  if (step + 1 > MAX_OPERAND) return -1;
  *var = slot - target->base;
  return INST(steps[op - LESS_THAN], OPERANDS(slot + 1, -(step + 1), OPERAND_RIGHT(header)));
}

static void compile_recur(Snap* snap, SnapFnState* fs, SCons* args, bool tail) {
  int i, step, var = -1;
  bool stored[MAX_LOCALS];
  SCons* curr;
  SnapLoop* target = fs->loop;

  if (!target) {
    parse_raise(snap, "snap_compile_error", "recur must be inside a fn or loop");
  }
  if (!tail) {
    parse_raise(snap, "snap_compile_error", "recur must be in tail position");
  }
  if (list_length(args) != target->count) {
    parse_raise(snap, "snap_compile_error", "Invalid number of arguments");
  }

  step = step_inst(snap, fs, target, args, &var);

  // Each value is stored as soon as it's computed unless a later argument
  // reads the variable, otherwise it's left on the stack and stored after.
  // Captured variables must be closed before they are overwritten, so
  // everything is stored after then. A stepped variable is left to STEP.
  for (i = 0, curr = args; curr != NULL; ++i, curr = as_cons(curr->rest)) {
    int slot = target->base + i;
    SCons* later;
    stored[i] = i == var || is_same_local(fs, curr->first, slot);
    if (stored[i]) continue;
    compile(snap, fs, curr->first, false);
    stored[i] = !target->closes;
    for (later = as_cons(curr->rest); stored[i] && later; later = as_cons(later->rest)) {
      stored[i] = !references(later->first, fs->locals[slot].name);
    }
    if (stored[i]) emit(fs, STORE_LOCAL, slot);
  }

  if (target->closes) {
    emit(fs, CLOSE_UPVALS, target->base);
  }

  for (i = target->count - 1; i >= 0; --i) {
    if (!stored[i]) emit(fs, STORE_LOCAL, target->base + i);
  }

  // STEP is followed by the jump back, which it skips when the compare holds
  if (step >= 0) *mvec_push(&fs->insts, SnapInst) = step;
  emit_jump_back(snap, fs, target->top);
}

static void compile_panic(Snap* snap, SnapFnState* fs, SCons* args) {
  if (list_length(args) != 2) {
    parse_raise(snap, "snap_compile_error", "Expected (panic <err> <msg>)");
  }
  compile(snap, fs, list_nth(args, 1), false);
  compile(snap, fs, args->first, false);
  emit(fs, PANIC, 0);
}


// Vectors are mutable, so a literal makes a new one each time it's evaluated
static void compile_vec(Snap* snap, SnapFnState* fs, SVec* vec) {
  int i;
  for (i = 0; i < vec->len; ++i) {
    compile(snap, fs, *snap_vec_at(vec, i), false);
  }
  load_constant(snap, fs, create_cfunc(builtin_vector));
  emit(fs, CALL, vec->len);
}

// Like vectors, a literal makes a new map each time
static void compile_map(Snap* snap, SnapFnState* fs, SMap* map) {
  int i;
  for (i = 0; i < map->used; ++i) {
    if (map->entries[2 * i].bits == SV_DELETED) continue;
    compile(snap, fs, map->entries[2 * i], false);
    compile(snap, fs, map->entries[2 * i + 1], false);
  }
  load_constant(snap, fs, create_cfunc(builtin_map));
  emit(fs, CALL, 2 * map->len);
}

static void compile_recover(Snap* snap, SnapFnState* fs, SCons* args) {
  if (list_length(args) != 1) {
    parse_raise(snap, "snap_compile_error", "Expected (recover <fn>)");
  }
  compile(snap, fs, args->first, false);
  emit(fs, RECOVER, 0);
}

static void compile_try(Snap* snap, SnapFnState* fs, SCons* args) {
  if (list_length(args) != 1) {
    parse_raise(snap, "snap_compile_error", "Expected (try <expr>)");
  }
  compile(snap, fs, args->first, false);
  emit(fs, TRY, 0);
}

static void compile_arith(Snap* snap, SnapFnState* fs, int opcode, SCons* args) {
  SCons* curr;
  if ((opcode == SUB || opcode == ADD) && list_length(args) == 1) { // (- x) is (- 0 x), (+ x) is (+ 0 x)
    compile_binary(snap, fs, opcode, create_int(0), args->first);
    return;
  }
  if (list_length(args) < 2) {
    parse_raise(snap, "snap_compile_error", "Expected expressions");
  }
  compile_binary(snap, fs, opcode, args->first, list_nth(args, 1));
  // Left fold: (- a b c) is ((a - b) - c)
  for (curr = as_cons(as_cons(args->rest)->rest); curr != NULL; curr = as_cons(curr->rest)) {
    compile(snap, fs, curr->first, false);
    emit(fs, opcode, 0);
  }
}

// Stack on entry: [prev]. Leaves [last result] on every path.
static void compile_compare_rest(Snap* snap, SnapFnState* fs, int opcode, SCons* curr) {
  int jump;
  compile(snap, fs, curr->first, false); // [prev x]
  emit(fs, DUP, 0);                      // [prev x x]
  emit(fs, ROT3, 0);                     // [x prev x]
  emit(fs, opcode, 0);                   // [x result]
  if (is_empty(curr->rest)) return;
  jump = emit_jump(fs, JUMP_FALSE_OR_POP); // Stop at the first false: [x false]
  compile_compare_rest(snap, fs, opcode, as_cons(curr->rest));
  patch_jump(snap, fs, jump);
}

static void compile_compare(Snap* snap, SnapFnState* fs, int opcode, SCons* args) {
  int count = list_length(args);
  if (count < 2) {
    parse_raise(snap, "snap_compile_error", "Expected expressions");
  }
  if (count == 2) {
    compile_binary(snap, fs, opcode, args->first, list_nth(args, 1));
  } else {
    compile(snap, fs, args->first, false);
    // (< a b c) is (and (< a b) (< b c)) evaluating each operand once
    compile_compare_rest(snap, fs, opcode, as_cons(args->rest)); // [x result]
    emit(fs, ROT2, 0);
    emit(fs, POP, 0);
  }
}

static void compile_and_or(Snap* snap, SnapFnState* fs, int opcode, SCons* args, bool tail) {
  int jump;
  compile(snap, fs, args->first, tail && is_empty(args->rest));
  if (is_empty(args->rest)) return;
  jump = emit_jump(fs, opcode);
  compile_and_or(snap, fs, opcode, as_cons(args->rest), tail);
  patch_jump(snap, fs, jump);
}

static void compile_call(Snap* snap, SnapFnState* fs, SCons* sexpr) {
  SCons* curr;
  int count = 0;
  for (curr = as_cons(sexpr->rest); curr != NULL; curr = as_cons(curr->rest)) {
    compile(snap, fs, curr->first, false);
    count++;
  }
  compile(snap, fs, sexpr->first, false);
  emit(fs, CALL, count);
}

// Compiles an expression whose value isn't used, leaving nothing on the stack.
// Then an if (or a when or cond, once expanded) doesn't need to push nil for
// a missing branch, then pop it.
static void compile_effect(Snap* snap, SnapFnState* fs, SValue expr) {
  SCons* sexpr = snap_type(expr) == STYPE_CONS ? as_cons(expr) : NULL;
  SCons* args = sexpr ? as_cons(sexpr->rest) : NULL;
  SValue macro, form;
  parse_enter(snap, "snap_compile_error");
  if (sexpr && is_sym(sexpr->first) && find_macro(fs, sexpr->first, &macro)) {
    form = macro_expand(snap, macro, args);
    if (is_err(form)) {
      snap->cause = as_err(form);
      longjmp(snap->jmp, 1);
    }
    if (is_obj(form)) anchor(snap, as_obj(form));
    compile_effect(snap, fs, form);
    if (is_obj(form)) release(snap);
  } else if (sexpr && is_form(sexpr->first) && as_form(sexpr->first) == TK_IF && list_length(args) == 3) {
    compile_branches(snap, fs, args, false, true);
  } else if (sexpr && is_form(sexpr->first) && as_form(sexpr->first) == TK_BEGIN && args) {
    for (; args; args = as_cons(args->rest)) compile_effect(snap, fs, args->first);
  } else {
    compile(snap, fs, expr, false);
    emit(fs, POP, 0);
  }
  snap->c_depth--;
}

static const char* const inline_names[SNAP_NUM_INLINE_FNS] = { "some", "every?", "map", "filter", "reduce", "keep", "remove", "map-indexed", "mapcat" };
enum { INLINE_SOME, INLINE_EVERY, INLINE_MAP, INLINE_FILTER, INLINE_REDUCE, INLINE_KEEP, INLINE_REMOVE, INLINE_MAP_INDEXED, INLINE_MAPCAT };

// Called once stdlib.snap has run: the functions inline_seq() inlines
void compile_init_inline(Snap* snap) {
  int k;
  for (k = 0; k < SNAP_NUM_INLINE_FNS; ++k) {
    snap->inline_syms[k] = snap_sym_new(snap, inline_names[k]);
    snap->inline_fns[k] = snap->inline_syms[k]->value; // A GC root, so no other
                                                        // function gets its address
  }
}

// Whether name is a global here, not a local or closed over variable
static bool is_global_name(SnapFnState* fs, SSym* name) {
  for (; fs; fs = fs->up) {
    if (local_entry(fs, name) >= 0) return false;
    if (fs->top || fs->inlining) break;
  }
  return true;
}

// A list of the n values, anchored until the caller unwinds the anchors
static SValue make_list(Snap* snap, int n, ...) {
  SnapList list = { NULL, NULL };
  SValue result;
  va_list args;
  va_start(args, n);
  while (n-- > 0) *list_add(snap, &list) = va_arg(args, SValue);
  va_end(args);
  result = list_done(snap, &list);
  anchor(snap, as_obj(result));
  return result;
}

// A vector of the n values, anchored like make_list()'s
static SValue make_vec(Snap* snap, int n, ...) {
  SVec* vec = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, n));
  va_list args;
  va_start(args, n);
  while (n-- > 0) vec_push(snap, vec, va_arg(args, SValue));
  va_end(args);
  return create_obj(vec);
}

// (some (fn [x] body...) coll), and the same with every?, map (of one
// collection), filter, reduce, keep, remove, map-indexed and mapcat, runs the loop in place, with the fn's
// parameters bound by a let, so there's no closure to make and no calls:
//   (let [#c coll]
//     (if (#inline k #c)
//       (let [#n (count #c)]
//         (loop [#i 0]
//           (if (< #i #n)
//             (let [#r (let [x (nth #c #i)] body...)] (if #r #r (recur (+ #i 1))))
//             nil)))
//       (some (fn [x] body...) #c)))
// #inline checks that the global is still the stdlib function and that the
// collection is a vector or map; otherwise the call is made as written. The
// loops are stdlib.snap's, using the same globals. coll is evaluated before
// the fn is made, which nothing can tell. A fn that takes the rest of its
// arguments or has a recur stays a call.
static bool inline_seq(Snap* snap, SnapFnState* fs, SCons* sexpr, bool tail) {
  static const int num_params[SNAP_NUM_INLINE_FNS] = { 1, 1, 1, 1, 2, 1, 1, 2, 1 };
  SSym* name = as_sym(sexpr->first);
  SSym* count = snap_sym_new(snap, "count");
  SSym* nth = snap_sym_new(snap, "nth");
  SSym* push = snap_sym_new(snap, "push!");
  SSym* is_vector = snap_sym_new(snap, "vector?");
  SSym* to_vec = snap_sym_new(snap, "vec");
  SSym* amp = snap_sym_new(snap, "&");
  SCons* args = as_cons(sexpr->rest);
  SCons* fn_args;
  SVec* params;
  SValue c, n, i, a, x, out, item, call, loop, fast, slow, form;
  SValue one = create_int(1), zero = create_int(0);
  SCons* saved_plain;
  int k, j, num_anchors = snap->anchors.vsize;

  for (k = 0; k < SNAP_NUM_INLINE_FNS && snap->inline_syms[k] != name; ++k) {}
  if (k == SNAP_NUM_INLINE_FNS || name->value.bits != snap->inline_fns[k].bits) return false;
  if (list_length(args) != num_params[k] + 1 || !is_global_name(fs, name)) return false;
  if (!is_cons(args->first) || !as_cons(args->first) || !is_form(as_cons(args->first)->first) ||
      as_form(as_cons(args->first)->first) != TK_FN) {
    return false;
  }
  fn_args = as_cons(as_cons(args->first)->rest);
  if (!fn_args || !is_vec(fn_args->first) || !is_cons(fn_args->rest) || !as_cons(fn_args->rest)) return false;
  params = as_vec(fn_args->first);
  if (params->len != num_params[k] || contains_form(fn_args->rest, TK_RECUR)) return false;
  // The loop's own calls must reach the globals, from where they are
  if (!is_global_name(fs, count) || !is_global_name(fs, nth) || !is_global_name(fs, push)) return false;
  if (k == INLINE_MAPCAT && (!is_global_name(fs, is_vector) || !is_global_name(fs, to_vec))) return false;
  for (j = 0; j < params->len; ++j) {
    SValue p = *snap_vec_at(params, j);
    if (binds(snap, p, amp) || binds(snap, p, count) || binds(snap, p, nth) || binds(snap, p, push)) return false;
    if (k == INLINE_MAPCAT && (binds(snap, p, is_vector) || binds(snap, p, to_vec))) return false;
  }

  c = new_gensym(snap);
  n = new_gensym(snap);
  i = new_gensym(snap);
  item = make_list(snap, 3, create_obj(nth), c, i);
  a = x = create_nil();
  out = new_gensym(snap);
  if (k == INLINE_REDUCE) {
    a = new_gensym(snap); // The initial value; call is made below
    call = create_nil();
  } else if (k == INLINE_MAP_INDEXED) {
    call = make_form(snap, create_form(TK_LET), make_vec(snap, 4, *snap_vec_at(params, 0), i, *snap_vec_at(params, 1), item),
                     fn_args->rest);
    anchor(snap, as_obj(call));
  } else if (k == INLINE_FILTER || k == INLINE_REMOVE) {
    x = new_gensym(snap);
    call = make_form(snap, create_form(TK_LET), make_vec(snap, 2, *snap_vec_at(params, 0), x), fn_args->rest);
    anchor(snap, as_obj(call));
  } else {
    call = make_form(snap, create_form(TK_LET), make_vec(snap, 2, *snap_vec_at(params, 0), item), fn_args->rest);
    anchor(snap, as_obj(call));
  }

  {
    SValue more = make_list(snap, 3, create_form(TK_LT), i, n);
    SValue next = make_list(snap, 2, create_form(TK_RECUR), make_list(snap, 3, create_form(TK_ADD), i, one));
    SValue bindings = make_vec(snap, 2, i, zero);
    switch (k) {
      case INLINE_SOME: {
        SValue r = new_gensym(snap);
        loop = make_list(snap, 4, create_form(TK_IF), more,
                         make_list(snap, 3, create_form(TK_LET), make_vec(snap, 2, r, call),
                                   make_list(snap, 4, create_form(TK_IF), r, r, next)),
                         create_nil());
        break;
      }
      case INLINE_EVERY:
        loop = make_list(snap, 4, create_form(TK_IF), more,
                         make_list(snap, 4, create_form(TK_IF), call, next, create_bool(false)),
                         create_bool(true));
        break;
      case INLINE_MAP: case INLINE_MAP_INDEXED:
        loop = make_list(snap, 4, create_form(TK_IF), more,
                         make_list(snap, 3, create_form(TK_BEGIN), make_list(snap, 3, create_obj(push), out, call), next),
                         out);
        break;
      case INLINE_FILTER:
        loop = make_list(snap, 4, create_form(TK_IF), more,
                         make_list(snap, 4, create_form(TK_LET), make_vec(snap, 2, x, item),
                                   make_list(snap, 4, create_form(TK_IF), call,
                                             make_list(snap, 3, create_obj(push), out, x), create_nil()),
                                   next),
                         out);
        break;
      case INLINE_REMOVE:
        loop = make_list(snap, 4, create_form(TK_IF), more,
                         make_list(snap, 4, create_form(TK_LET), make_vec(snap, 2, x, item),
                                   make_list(snap, 4, create_form(TK_IF), call, create_nil(),
                                             make_list(snap, 3, create_obj(push), out, x)),
                                   next),
                         out);
        break;
      case INLINE_KEEP: {
        // (let [#r call] (if (= #r nil) nil (push! out #r)) (recur ...))
        SValue r = new_gensym(snap);
        loop = make_list(snap, 4, create_form(TK_IF), more,
                         make_list(snap, 4, create_form(TK_LET), make_vec(snap, 2, r, call),
                                   make_list(snap, 4, create_form(TK_IF), make_list(snap, 3, create_form(TK_EQ), r, create_nil()),
                                             create_nil(), make_list(snap, 3, create_obj(push), out, r)),
                                   next),
                         out);
        break;
      }
      case INLINE_MAPCAT: {
        // (let [#r call #r (if (vector? #r) #r (vec #r)) #m (count #r)]
        //   (loop [#j 0] (if (< #j #m) (begin (push! out (nth #r #j)) (recur (+ #j 1))) nil))
        //   (recur ...))
        SValue r = new_gensym(snap), m = new_gensym(snap), j = new_gensym(snap), inner;
        inner = make_list(snap, 4, create_form(TK_IF), make_list(snap, 3, create_form(TK_LT), j, m),
                          make_list(snap, 3, create_form(TK_BEGIN),
                                    make_list(snap, 3, create_obj(push), out, make_list(snap, 3, create_obj(nth), r, j)),
                                    make_list(snap, 2, create_form(TK_RECUR), make_list(snap, 3, create_form(TK_ADD), j, one))),
                          create_nil());
        inner = make_list(snap, 3, create_form(TK_LOOP), make_vec(snap, 2, j, zero), inner);
        loop = make_list(snap, 4, create_form(TK_IF), more,
                         make_list(snap, 4, create_form(TK_LET),
                                   make_vec(snap, 6, r, call,
                                            r, make_list(snap, 4, create_form(TK_IF), make_list(snap, 2, create_obj(is_vector), r), r,
                                                         make_list(snap, 2, create_obj(to_vec), r)),
                                            m, make_list(snap, 2, create_obj(count), r)),
                                   inner, next),
                         out);
        break;
      }
      default: { // INLINE_REDUCE
        SValue acc = new_gensym(snap);
        call = make_form(snap, create_form(TK_LET),
                         make_vec(snap, 4, *snap_vec_at(params, 0), acc, *snap_vec_at(params, 1), item),
                         fn_args->rest);
        anchor(snap, as_obj(call));
        bindings = make_vec(snap, 4, acc, a, i, zero);
        loop = make_list(snap, 4, create_form(TK_IF), more,
                         make_list(snap, 3, create_form(TK_RECUR), call, make_list(snap, 3, create_form(TK_ADD), i, one)),
                         acc);
        break;
      }
    }
    loop = make_list(snap, 3, create_form(TK_LOOP), bindings, loop);
  }
  fast = make_list(snap, 3, create_form(TK_LET),
                   (k != INLINE_SOME && k != INLINE_EVERY && k != INLINE_REDUCE)
                     ? make_vec(snap, 4, n, make_list(snap, 2, create_obj(count), c), out, make_vec(snap, 0))
                     : make_vec(snap, 2, n, make_list(snap, 2, create_obj(count), c)),
                   loop);
  slow = k == INLINE_REDUCE ? make_list(snap, 4, sexpr->first, args->first, a, c)
                            : make_list(snap, 3, sexpr->first, args->first, c);
  form = make_list(snap, 4, create_form(TK_IF),
                   make_list(snap, 3, create_form(TK_INLINE), create_int(k), c), fast, slow);
  form = make_list(snap, 3, create_form(TK_LET),
                   k == INLINE_REDUCE ? make_vec(snap, 4, a, list_nth(args, 1), c, list_nth(args, 2))
                                      : make_vec(snap, 2, c, list_nth(args, 1)),
                   form);

  saved_plain = fs->plain_call;
  fs->plain_call = as_cons(slow);
  compile(snap, fs, form, tail);
  fs->plain_call = saved_plain;
  snap->anchors.vsize = num_anchors;
  return true;
}

// Inlines a call to a global that holds a small function (see compile_fn):
//   GUARD_GLOBAL f   skips the next instruction if f still holds the function
//   JUMP slow
//   ...              the arguments in new locals, then the body with them
//                    as its parameters (an argument that's a literal, or a
//                    local the caller never assigns, is used where it is)
//   JUMP end
//   slow: (f args...), called as written
// While the body compiles, the caller's locals can't be seen, so its names
// mean what they mean in the function: its parameters, its own lets and
// globals. Inlining goes 2 deep, and not into a function's own body.
static bool inline_global(Snap* snap, SnapFnState* fs, SCons* sexpr) {
  SSym* name = as_sym(sexpr->first);
  SValue fn = name->value;
  SCode* code = is_closure(fn) ? as_closure(fn)->code : is_code(fn) ? as_code(fn) : NULL;
  SCons *form, *body, *arg;
  SVec* params;
  SnapLoop* loop;
  bool sets;
  int k, sym_k, fn_k, slow, end, stack, first, barrier;

  if (!code || code->inline_form < 0 || code->insts_count > INLINE_SIZE || as_obj(fn)->macro || name->read_only) {
    return false;
  }
  if (fs->inlining >= 2 || !is_global_name(fs, name)) return false;
  for (k = 0; k < fs->inlining; ++k) {
    if (fs->inlined[k] == code) return false;
  }
  form = as_cons(code->constants[code->inline_form]);
  params = as_vec(form->first);
  body = as_cons(form->rest);
  if (list_length(as_cons(sexpr->rest)) != params->len) return false;
  sym_k = add_constant(snap, fs, create_obj(name));
  fn_k = add_global_constant(snap, fs, fn);
  if (sym_k >= 2048 || fn_k >= 4096) return false;

  emit(fs, GUARD_GLOBAL, sym_k | fn_k << 11);
  slow = emit_jump(fs, JUMP);
  stack = fs->stack;
  sets = contains_form(create_obj(body), TK_SET);
  begin_scope(fs);
  first = fs->num_locals;
  // The arguments, in the caller's scope, then named as the parameters
  for (k = 0, arg = as_cons(sexpr->rest); arg; arg = as_cons(arg->rest), ++k) {
    SSym* p = binding_name(snap, *snap_vec_at(params, k));
    int index = is_sym(arg->first) ? resolve_local(fs, as_sym(arg->first)) : -1;
    int type = snap_type(arg->first);
    bool literal = type == STYPE_INT || type == STYPE_FLOAT || type == STYPE_STR || type == STYPE_BOOL ||
                   type == STYPE_KEY || type == STYPE_NIL;
    if (is_sym(arg->first) && constant_local(fs, as_sym(arg->first)) >= 0) {
      index = constant_local(fs, as_sym(arg->first)); // A parameter that's a constant, inlined again
      literal = true;
    } else if (literal) {
      index = add_constant(snap, fs, arg->first);
    }
    if ((literal || (index >= 0 && !fs->locals[index].assigned)) && !p->assigned && !sets) {
      SnapLocal* local = &fs->locals[add_local(snap, fs, as_sym(new_gensym(snap)), true)];
      if (literal) local->constant = index;
      else local->alias = index;
    } else {
      compile(snap, fs, arg->first, false);
      emit(fs, STORE_LOCAL, add_local(snap, fs, as_sym(new_gensym(snap)), true));
    }
  }
  for (k = 0; k < params->len; ++k) {
    SnapLocal* local = &fs->locals[first + k];
    local->name = binding_name(snap, *snap_vec_at(params, k));
    local->assigned = local->name->assigned || sets;
  }
  scan_sets(fs, create_obj(body)); // So locals the body assigns aren't used in place either
  barrier = fs->barrier;
  loop = fs->loop;
  fs->barrier = first;
  fs->loop = NULL;
  fs->inlined[fs->inlining++] = code;
  compile_body(snap, fs, body, false);
  fs->inlining--;
  fs->loop = loop;
  fs->barrier = barrier;
  end_scope(fs);
  end = emit_jump(fs, JUMP);

  patch_jump(snap, fs, slow);
  fs->stack = stack;
  compile_call(snap, fs, sexpr);
  patch_jump(snap, fs, end);
  return true;
}

static void compile_sexpr(Snap* snap, SnapFnState* fs, SCons* sexpr, bool tail) {
  SCons* args = as_cons(sexpr->rest);
  SValue macro, form;
  switch (snap_type(sexpr->first)) {
    case STYPE_SYM:
      if (find_macro(fs, sexpr->first, &macro)) {
        form = macro_expand(snap, macro, args);
        if (is_err(form)) {
          snap->cause = as_err(form);
          longjmp(snap->jmp, 1);
        }
        if (is_obj(form)) anchor(snap, as_obj(form));
        compile(snap, fs, form, tail);
        if (is_obj(form)) release(snap);
        break;
      }
      if (sexpr != fs->plain_call && inline_seq(snap, fs, sexpr, tail)) break;
      if (inline_global(snap, fs, sexpr)) break;
      compile_call(snap, fs, sexpr);
      break;
    case STYPE_CONS:
    case STYPE_CFUNC: // From quasiquote
      compile_call(snap, fs, sexpr);
      break;
    case STYPE_KEY: {
      // (:k x) is (get x :k), and (:k x d) is (get x :k d): the same call,
      // so the same instruction (see GET)
      SnapList list = { NULL, NULL };
      int n = list_length(args);
      if (n < 1 || n > 2) {
        parse_raise(snap, "snap_compile_error", "Expected (:%s m) or (:%s m default)", as_key(sexpr->first)->data,
              as_key(sexpr->first)->data);
      }
      *list_add(snap, &list) = create_obj(snap_sym_new(snap, "get"));
      *list_add(snap, &list) = args->first;
      *list_add(snap, &list) = sexpr->first;
      if (n == 2) *list_add(snap, &list) = list_nth(args, 1);
      form = list_done(snap, &list);
      anchor(snap, as_obj(form));
      compile_call(snap, fs, as_cons(form));
      release(snap);
      break;
    }
    case STYPE_FORM:
      switch (as_form(sexpr->first)) {
        case TK_BEGIN:
          if (!args) {
            parse_raise(snap, "snap_compile_error", "Expected expressions");
          }
          compile_body(snap, fs, args, tail);
          break;
        case TK_DEF:
          compile_def(snap, fs, args);
          break;
        case TK_DEFMACRO:
          compile_defmacro(snap, fs, args);
          break;
        case TK_INLINE:
          compile(snap, fs, list_nth(args, 1), false);
          emit(fs, INLINE_GUARD, as_int(args->first));
          break;
        case TK_UNQUOTE:
        case TK_SPLICE:
          parse_raise(snap, "snap_compile_error", "~ and ~@ must be in a quasiquote (`)");
          break;
        case TK_IF:
          compile_if(snap, fs, args, tail);
          break;
        case TK_FN:
          compile_fn(snap, fs, args);
          break;
        case TK_LET:
          compile_let(snap, fs, args, false, tail);
          break;
        case TK_LOOP:
          compile_let(snap, fs, args, true, tail);
          break;
        case TK_QUOTE:
          if (list_length(args) != 1) {
            parse_raise(snap, "snap_compile_error", "Expected a single expression");
          }
          make_read_only(args->first);
          load_constant(snap, fs, args->first);
          break;
        case TK_RECUR:
          compile_recur(snap, fs, args, tail);
          break;
        case TK_SET:
          compile_set(snap, fs, args);
          break;
        case TK_PANIC:
          compile_panic(snap, fs, args);
          break;
        case TK_TRY:
          compile_try(snap, fs, args);
          break;
        case TK_RECOVER:
          compile_recover(snap, fs, args);
          break;
        case TK_ADD: case TK_SUB: case TK_MUL: case TK_DIV: case TK_MOD:
          compile_arith(snap, fs, (as_form(sexpr->first) - TK_ADD) + ADD, args);
          break;
        case TK_LT: case TK_LE: case TK_GT: case TK_GE: case TK_EQ:
          compile_compare(snap, fs, (as_form(sexpr->first) - TK_LT) + LESS_THAN, args);
          break;
        case TK_NOT:
          if (list_length(args) != 1) {
            parse_raise(snap, "snap_compile_error", "Expected expression");
          }
          compile(snap, fs, args->first, false);
          emit(fs, NOT, 0);
          break;
        case TK_NE:
          compile_compare(snap, fs, EQUALS, args);
          emit(fs, NOT, 0);
          break;
        case TK_AND: case TK_OR:
          if (!args) {
            load_constant(snap, fs, create_bool(as_form(sexpr->first) == TK_AND));
          } else {
            compile_and_or(snap, fs, as_form(sexpr->first) == TK_AND ? JUMP_FALSE_OR_POP
                                                              : JUMP_TRUE_OR_POP,
                           args, tail);
          }
          break;
        default:
          parse_raise(snap, "snap_compile_error", "Unexpected special form");
          break;
      }
      break;
    default:
      parse_raise(snap, "snap_compile_error", "Expected id, special form or expr");
      break;
  }
}

// `tail` is whether expr is in tail position relative to its recur target

static void compile(Snap* snap, SnapFnState* fs, SValue expr, bool tail) {
  int stack = fs->stack;
  SValue val;
  parse_enter(snap, "snap_compile_error");
  switch (snap_type(expr)) {
    case STYPE_INT:
    case STYPE_FLOAT:
    case STYPE_STR:
    case STYPE_BOOL:
    case STYPE_KEY:
    case STYPE_CFUNC: // From quasiquote
      load_constant(snap, fs, expr);
      break;
    case STYPE_NIL:
      emit(fs, LOAD_NIL, 0);
      break;
    case STYPE_SYM:
      load_variable(snap, fs, as_sym(expr));
      break;
    case STYPE_VEC:
      compile_vec(snap, fs, as_vec(expr));
      break;
    case STYPE_MAP:
      compile_map(snap, fs, as_map(expr));
      break;
    case STYPE_CONS:
      if (is_empty(expr)) {
        load_constant(snap, fs, expr);
      } else {
        compile_sexpr(snap, fs, as_cons(expr), tail);
      }
      break;
    case STYPE_FORM:
      // An operator that isn't called, like the + in (reduce + 0 v), is a C
      // function that does the same. Calls still compile to instructions.
      if (!op_value(as_form(expr), &val)) {
        parse_raise(snap, "snap_compile_error", "%s can't be used as a value", form_name(as_form(expr)));
      }
      load_constant(snap, fs, val);
      break;
    default:
      parse_raise(snap, "snap_compile_error", "Unexpected type");
      break;
  }
  // Every expression leaves exactly one value (even when it ends in a jump,
  // like recur, or never finishes, like panic)
  fs->stack = stack;
  adjust_stack(fs, 1);
  snap->c_depth--;
}

// Compiles the forms from lex into a function without arguments: all of
// them, or just the next one. A macro can compile code (with the compile
// built-in) while its caller is being compiled, so this saves and restores the
// compiler's state.
SValue compile_forms(Snap* snap, SnapLex* lex, bool all) {
  SValue expr, result;
  SnapFnState* fs;
  SnapFnState* up = snap->fn_state;
  SCode* code;
  volatile bool first = true;
  int num_anchors = snap->anchors.vsize, c_depth = snap->c_depth;
  SnapLex lex_start = *lex;
  jmp_buf jmp;

  memcpy(jmp, snap->jmp, sizeof(jmp_buf));
start:
  if (setjmp(snap->jmp) > 0) {
    while (snap->fn_state != up) fn_state_free(snap);
    snap->c_depth = c_depth;
    if (snap->compile_restart) { // See assign_local()
      snap->compile_restart = false;
      snap->anchors.vsize = num_anchors;
      *lex = lex_start;
      first = true;
      goto start;
    }
    result = create_obj(snap->cause);
    goto done;
  }

  fs = fn_state_new(snap);
  fs->top = true;
  while ((all || first) && parse_form(snap, lex, &expr)) {
    if (is_obj(expr)) anchor(snap, as_obj(expr));
    scan_sets(fs, expr);
#ifdef SNAP_DEBUG
    snap_print(expr);
    printf("\n");
#endif
    if (!first) emit(fs, POP, 0);
    first = false;
    compile(snap, fs, expr, false);
    if (is_obj(expr)) release(snap);
  }
  if (first) {
    fn_state_free(snap);
    result = create_nil();
    goto done;
  }
  emit(fs, RETURN, 0);

  code = code_new(snap, fs);
  fn_state_free(snap);
  if (snap->dump_code) print_code(code, 0);
  result = create_obj(code);

done:
  memcpy(snap->jmp, jmp, sizeof(jmp_buf));
  snap->anchors.vsize = num_anchors; // Also unwinds anchors left by a longjmp
  return result;
}
static void iprintf(int indent, const char* format, ...) {
  va_list args;
  va_start(args, format);
  printf("%*s", indent, "");
  vprintf(format, args);
  va_end(args);
}

const char* form_name(int token) {
  switch (token) {
    case TK_BEGIN: return "begin";
    case TK_DEF: return "def";
    case TK_DEFMACRO: return "defmacro";
    case TK_UNQUOTE: return "unquote";
    case TK_SPLICE: return "splice";
    case TK_INLINE: return "#inline";
    case TK_ELLIPSIS: return "...";
    case TK_IF: return "if";
    case TK_FN: return "fn";
    case TK_LET: return "let";
    case TK_LOOP: return "loop";
    case TK_QUOTE: return "quote";
    case TK_RECUR: return "recur";
    case TK_SET: return "set!";
    case TK_PANIC: return "panic";
    case TK_TRY: return "try";
    case TK_RECOVER: return "recover";
    case TK_LT: return "<";
    case TK_LE: return "<=";
    case TK_GT: return ">";
    case TK_GE: return ">=";
    case TK_EQ: return "=";
    case TK_NOT: return "not";
    case TK_NE: return "not=";
    case TK_AND: return "and";
    case TK_OR: return "or";
    case TK_ADD: return "+";
    case TK_SUB: return "-";
    case TK_MUL: return "*";
    case TK_DIV: return "/";
    case TK_MOD: return "%";
    default: return "<form>";
  }
}

static const char* opcode_name(int op) {
  switch (op) {
#define XX(opcode) \
    case opcode: \
      return #opcode;
    OPCODE_MAP(XX)
#undef XX
    default:
      return "INVALID";
  }
}

static void print_code(SCode* code, int indent) {
  int i;
  iprintf(indent, "args: %d, locals: %d, max stack: %d\n",
          code->num_args, code->num_locals, code->max_stack_size);
  for (i = 0; i < code->num_upvals; ++i) {
    iprintf(indent, "upval %d: %s %d%s\n", i,
            (code->upvals[i] & 1) ? "local" : "upval", code->upvals[i] >> 2,
            (code->upvals[i] & 2) ? " (copied)" : "");
  }
  for (i = 0; i < code->insts_count; ++i) {
    int op = variant_base(INST_OP(code->insts[i]));
    int arg = INST_ARG(code->insts[i]);
    iprintf(indent, "%6d %-18s", i, opcode_name(op));
    if (is_binary_op(op)) {
      int operands[2] = { OPERAND_LEFT(code->insts[i]), OPERAND_RIGHT(code->insts[i]) };
      int j;
      for (j = 0; j < 2; ++j) {
        if (operands[j] > 0) printf(" L%d", operands[j] - 1);
        else if (operands[j] < 0) printf(" K%d", -operands[j] - 1);
        else printf(" S");
      }
      if (OPERAND_DEST(code->insts[i])) printf(" -> L%d", OPERAND_DEST(code->insts[i]) - 1);
    }
    if (op >= STEP_LT && op <= STEP_GE) {
      // L += K, then compared with the right operand
      int right = OPERAND_RIGHT(code->insts[i]);
      printf(" L%d K%d", OPERAND_DEST(code->insts[i]) - 1, -OPERAND_LEFT(code->insts[i]) - 1);
      if (right > 0) printf(" L%d", right - 1);
      else printf(" K%d", -right - 1);
    }
    switch (op) {
      case LOAD_GLOBAL: case STORE_GLOBAL: case LOAD_CONSTANT:
        printf(" %-6d ", arg);
        print_val(stdout, code->constants[arg]);
        break;
      case JUMP: case JUMP_FALSE: case JUMP_TRUE: case JUMP_FALSE_OR_POP: case JUMP_TRUE_OR_POP:
        printf(" %-6d (to %d)", arg, i + 1 + arg);
        break;
      case LOAD_LOCAL: case STORE_LOCAL: case LOAD_CLOSED: case STORE_CLOSED: case LOAD_COPIED:
      case CALL: case CLOSE_UPVALS: case RETURN_VEC: case UNPACK: case INLINE_GUARD:
        printf(" %d", arg);
        break;
      case GUARD_GLOBAL:
        printf(" %-6d ", arg >> 11);
        print_val(stdout, code->constants[arg & 2047]);
        break;
      case CALL_GLOBAL: case NTH: case COUNT: case GET: case PUT: case INT: case MIN: case MAX:
        printf(" %-6d ", arg & 0xff);
        print_val(stdout, code->constants[arg >> 8]);
        break;
    }
    printf("\n");
  }
  for (i = 0; i < code->num_constants; ++i) {
    if (is_code(code->constants[i])) {
      iprintf(indent, "constant %d:\n", i);
      print_code(as_code(code->constants[i]), indent + 2);
    }
  }
}
