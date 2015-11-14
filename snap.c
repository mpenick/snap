#include "snap.h"
#include "snap_hash.h"
#include "snap_lex.h"

#include <assert.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define GC_EVERY_NUM_BYTES 64 * 1024 * 1024

#define list_entry(type, ptr, member) \
  ((type*)(((char*)ptr) - (uintptr_t)&((type*)0)->member))

#define list_foreach(list) \
  for (pos = (list)->next; pos != (list); pos = pos->next)

#define MAX_OP_ARG 33554431

enum {
  WHITE,
  GRAY,
  BLACK
};

#define OPCODE_MAP(XX) \
  XX(LOAD_GLOBAL, "LOAD_GLOBAL", true) \
  XX(STORE_GLOBAL, "STORE_GLOBAL", true) \
  XX(LOAD_LOCAL, "LOAD_LOCAL", true) \
  XX(STORE_LOCAL, "STORE_LOCAL", true) \
  XX(LOAD_CONSTANT, "LOAD_CONSTANT", true) \
  XX(LOAD_NIL, "LOAD_NIL", false) \
  XX(POP, "POP", false) \
  XX(CALL, "CALL", true) \
  XX(RETURN, "RETURN", false) \
  XX(JUMP, "JUMP", true) \
  XX(JUMP_FALSE, "JUMP_FALSE", true) \
  XX(ADD, "ADD", false) \
  XX(SUB, "SUB", false) \
  XX(MUL, "MUL", false) \
  XX(DIV, "DIV", false) \
  XX(MOD, "MOD", false) \
  XX(LESS_THAN, "LESS_THAN", false) \
  XX(LESS_EQUALS, "LESS_EQUALS", false) \
  XX(GREATER_THAN, "GREATER_THAN", false) \
  XX(GREATER_EQUALS, "GREATER_EQUALS", false) \
  XX(EQUALS, "EQUALS", false)

enum {
#define XX(opcode, name, has_arg) opcode,
  OPCODE_MAP(XX)
#undef XX
};

#define push(type, obj) (type*)snap_anchor(snap, (SObject*)obj)
#define push_sym(str) push(SSymStr, str)

static SValue exec(Snap* snap);
static SCode* parse(Snap* snap, SnapLex* lex);

static void list_init(SnapNode* list);
static void list_append(SnapNode* list, SnapNode* node);
static int list_forward_count(SnapNode* first, SnapNode* last);
static int list_backward_count(SnapNode* first, SnapNode* last);

static SInst* insts_append(Snap* snap, SCodeGen* code_gen, int opcode);
static int insts_calculate_jump(SInst* inst);
static void insts_remove_dead_code(SnapNode* insts);
static void insts_dump(SnapNode* insts);

static void print_val(SValue val, int indent);
static void print_code(SCode* code, int indent);
static void print_cons(SCons* cons, int indent);

static void jump_arg_init(SnapJumpArg* jump_arg, int dir, SnapNode* dest) {
  jump_arg->dir = dir;
  jump_arg->dest = dest;
}

static int arity(SCons* cons) {
  int n = 0;
  for (; cons; cons = as_cons(cons->rest)) n++;
  return n;
}

static void push_val(Snap* snap, SValue val) {
  if (is_obj(val)) snap_anchor(snap, val.o);
}

static void pop_val(Snap* snap, SValue val) {
  if (is_obj(val)) {
    assert(snap->anchored[snap->anchored_top - 1] == val.o);
    snap_release(snap);
  }
}

SValue create_undef() {
  SValue val;
  val.type = STYPE_UNDEF;
  val.o = NULL;
  return val;
}

SValue create_nil() {
  SValue val;
  val.type = STYPE_NIL;
  val.o = NULL;
  return val;
}

SValue create_empty() {
  SValue val;
  val.type = STYPE_CONS;
  val.o = NULL;
  return val;
}

SValue create_bool(bool b) {
  SValue val;
  val.type = STYPE_BOOL;
  val.b = b;
  return val;
}

SValue create_int(SnapInt i) {
  SValue val;
  val.type = STYPE_INT;
  val.i = i;
  return val;
}

SValue create_float(SnapFloat f) {
  SValue val;
  val.type = STYPE_FLOAT;
  val.f = f;
  return val;
}

SValue create_cfunc(SCFunc c) {
  SValue val;
  val.type = STYPE_CFUNC;
  val.c = c;
  return val;
}

SValue create_obj(SObject* o) {
  SValue val;
  val.type = o->type;
  val.o = o;
  return val;
}

static void gc_free(Snap* snap, SObject* obj) {
  //printf("free %p\n", obj);
  switch (obj->type) {
    case STYPE_SYM:
    case STYPE_STR:
      snap->num_bytes_alloced -= (sizeof(SSymStr) + ((SSymStr*)obj)->len + 1);
      break;
    case STYPE_ERR:
      snap->num_bytes_alloced -= sizeof(SErr);
      break;
    case STYPE_CONS:
      snap->num_bytes_alloced -= sizeof(SCons);
      break;
    case STYPE_ARR:
      snap->num_bytes_alloced -= (sizeof(SArr) + ((SArr*)obj)->len * sizeof(SValue));
      break;
    case STYPE_INST:
      snap->num_bytes_alloced -= sizeof(SInst);
      break;
    case STYPE_SCOPE:
      snap->num_bytes_alloced -= sizeof(SScope);
      snap_hash_destroy(&((SScope*)obj)->local_names);
      break;
    case STYPE_CODE_GEN:
      snap->num_bytes_alloced -= sizeof(SCodeGen);
      snap_hash_destroy(&((SCodeGen*)obj)->global_names);
      snap_hash_destroy(&((SCodeGen*)obj)->constants);
      snap_vec_destroy(&((SCodeGen*)obj)->param_names);
      break;
    case STYPE_CODE:
      snap->num_bytes_alloced -= sizeof(SCode);
      free(((SCode*)obj)->insts);
      break;
    default:
      assert(0 && "Not a GC object");
      break;
  }
  free(obj);
}

static void gc_mark(Snap* snap, SObject* obj) {
  if (!obj) return;
  switch (obj->type) {
    case STYPE_SYM:
    case STYPE_STR:
      obj->mark = BLACK;
      break;
    case STYPE_ERR:
    case STYPE_CONS:
    case STYPE_ARR:
    case STYPE_SCOPE:
    case STYPE_CODE_GEN:
    case STYPE_CODE:
      if (obj->mark == WHITE) {
        obj->mark = GRAY;
        obj->gc_gray_next = snap->gray;
        snap->gray = obj;
      }
      break;
    default:
      assert(0 && "Not a GC object");
      break;
  }
}

static void gc_mark_val(Snap* snap, SValue val) {
  if (is_obj(val)) gc_mark(snap, val.o);
}

static void gc_mark_hash(Snap* snap, SnapHash* hash) {
  int i;
  for (i = 0; i < hash->capacity; ++i) {
    SEntry* entry = &hash->entries[i];
    if (!is_undef(entry->key)) {
      gc_mark_val(snap, entry->key);
      gc_mark_val(snap, entry->val);
    }
  }
}

static void gc_mark_vec(Snap* snap, SnapVec* vec) {
  int i;
  for (i = 0; i < vec->size; ++i) {
    SValue* item = &vec->items[i];
    if (!is_undef_p(item)) {
      gc_mark_val(snap, *item);
    }
  }
}

static void gc_mark_children(Snap* snap, SObject* obj) {
  int i;
  switch (obj->type) {
    case STYPE_ERR:
      gc_mark(snap, (SObject*)((SErr*)obj)->msg);
      gc_mark(snap, (SObject*)((SErr*)obj)->inner);
      break;
    case STYPE_CONS:
      gc_mark_val(snap, ((SCons*)obj)->first);
      gc_mark_val(snap, ((SCons*)obj)->rest);
      break;
    case STYPE_ARR:
      for (i = 0; i < ((SArr*)obj)->len; ++i) {
        gc_mark_val(snap, ((SArr*)obj)->data[i]);
      }
      break;
    case STYPE_SCOPE:
      gc_mark_hash(snap, &((SScope*)obj)->local_names);
      break;
    case STYPE_CODE_GEN:
      gc_mark(snap, (SObject*)((SCodeGen*)obj)->scope);
      gc_mark_hash(snap, &((SCodeGen*)obj)->constants);
      gc_mark_hash(snap, &((SCodeGen*)obj)->global_names);
      gc_mark_vec(snap, &((SCodeGen*)obj)->param_names);
      break;
    case STYPE_CODE:
      gc_mark(snap, (SObject*)((SCode*)obj)->constants);
      gc_mark(snap, (SObject*)((SCode*)obj)->global_names);
      break;
    default:
      assert(0 && "Not a valid gray object");
      break;
  }
}

static void gc_collect(Snap* snap) {
  int i;
  SObject** obj;

  for (i = 0; i < snap->anchored_top; ++i) {
    gc_mark(snap, snap->anchored[i]);
  }

  gc_mark_hash(snap, &snap->globals);
  gc_mark(snap, (SObject*)snap->cause);

  while (snap->gray) {
    SObject* temp = snap->gray;
    snap->gray = snap->gray->gc_gray_next;
    gc_mark_children(snap, temp);
  }

  obj = &snap->all;

  while (*obj) {
    if ((*obj)->mark == WHITE) {
      SObject* temp = *obj;
      *obj = (*obj)->gc_next;
      gc_free(snap, temp);
    } else {
      (*obj)->mark = WHITE;
      obj = &(*obj)->gc_next;
    }
  }
}

static SObject* gc_new(Snap* snap, uint8_t type, size_t size) {
  assert(size > sizeof(SObject));
  if (snap->num_bytes_alloced_last_gc > GC_EVERY_NUM_BYTES) {
    gc_collect(snap);
    snap->num_bytes_alloced_last_gc = 0;
  }
  SObject* obj = (SObject*)malloc(size);
  //printf("new %p (type: %d)\n", obj, type);
  obj->type = type;
  obj->mark = WHITE;
  obj->gc_gray_next = NULL;
  obj->gc_next = snap->all;
  snap->all = obj;
  snap->num_bytes_alloced += size;
  snap->num_bytes_alloced_last_gc += size;
  return obj;
}

SSymStr* snap_str_new(Snap* snap, const char* str) {
  size_t len = strlen(str);
  SSymStr* s = (SSymStr*)gc_new(snap, STYPE_STR, sizeof(SSymStr) + len + 1);
  s->len = len;
  strcpy(s->data, str);
  return s;
}

SSymStr* snap_sym_new(Snap* snap, const char* sym) {
  size_t len = strlen(sym);
  SSymStr* s = (SSymStr*)gc_new(snap, STYPE_SYM, sizeof(SSymStr) + len + 1);
  s->len = len;
  strcpy(s->data, sym);
  return s;
}

SErr* snap_err_new(Snap* snap, int code, const char* msg) {
  SErr* e = (SErr*)gc_new(snap, STYPE_ERR, sizeof(SErr));
  e->code = code;
  e->msg = NULL;
  e->inner = snap->cause;
  snap_anchor(snap, (SObject*)e);
  e->msg = snap_str_new(snap, msg);
  snap_release(snap);
  return e;
}

SCons* snap_cons_new(Snap* snap) {
 SCons* c = (SCons*)gc_new(snap,  STYPE_CONS, sizeof(SCons));
 c->first = create_nil();
 c->rest = create_empty();
 return c;
}

SArr* snap_arr_new(Snap* snap, int len) {
  SArr* a = (SArr*)gc_new(snap, STYPE_ARR, sizeof(SArr) + len * sizeof(SValue));
  a->len = len;
  return a;
}

SArr* arr_new_from_hash(Snap* snap, SnapHash* hash) {
  int i;
  SArr* a = snap_arr_new(snap, hash->count);
  for (i = 0; i < hash->capacity; ++i) {
    SEntry* entry = &hash->entries[i];
    if (!is_undef(entry->key)) {
      a->data[entry->val.i] = entry->key;
    }
  }
  return a;
}

SCode* snap_code_new(Snap* snap, SCodeGen* code_gen) {
  SCode* c = (SCode*)gc_new(snap, STYPE_CODE, sizeof(SCode));
  snap_anchor(snap, (SObject*)c);

  insts_dump(&code_gen->insts);
  printf("\n");
  insts_remove_dead_code(&code_gen->insts);
  insts_dump(&code_gen->insts);
  printf("\n");

  c->insts = (int*)malloc(sizeof(int) * code_gen->insts_count);
  {
    int count = 0;
    int stack_size = 0;
    int max_stack_size = 0;
    SnapNode* pos;
    list_foreach(&code_gen->insts) {
      SInst* inst = list_entry(SInst, pos, list);
      switch(inst->opcode) {
        case LOAD_GLOBAL: case LOAD_LOCAL:
        case LOAD_CONSTANT: case LOAD_NIL:
          stack_size++;
          if (stack_size > max_stack_size) {
            max_stack_size = stack_size;
          }
          break;
        case STORE_GLOBAL: case STORE_LOCAL:
          stack_size--;
          break;
        case POP:
          stack_size--;
          break;
        case CALL:
          stack_size -= inst->arg; // subtract arg + 1, add 1
          break;
        case RETURN:
          stack_size = 0;
          break;
        case JUMP:
          inst->arg = insts_calculate_jump(inst) + MAX_OP_ARG;
          stack_size--;
          break;
        case JUMP_FALSE:
          inst->arg = insts_calculate_jump(inst) + MAX_OP_ARG;
          stack_size--;
          break;
        case ADD: case SUB: case MUL: case DIV: case MOD:
          stack_size--; // subtract 2, add 1
          break;
      }
      c->insts[count++] = ((inst->opcode & 0x3F) << 26) | (inst->arg & 0x3FFFFFF);
    }
    c->num_locals = code_gen->num_locals;
    c->max_stack_size = max_stack_size;
    c->insts_count = count;
  }
  c->constants = arr_new_from_hash(snap, &code_gen->constants);
  c->global_names = arr_new_from_hash(snap, &code_gen->global_names);
  snap_release(snap);
  return c;
}

SInst* snap_inst_new(Snap* snap, int opcode) {
 SInst* i = (SInst*)gc_new(snap,  STYPE_INST, sizeof(SInst));
 i->opcode = opcode;
 i->arg = 0;
 jump_arg_init(&i->jump_arg, 0, NULL);
 i->list.next = i->list.prev = NULL;
 return i;
}

SScope* snap_scope_new(Snap* snap, SScope* up) {
  SScope* s = (SScope*)gc_new(snap,  STYPE_SCOPE, sizeof(SScope));
  snap_hash_init(&s->local_names);
  return s;
}

SCodeGen* snap_code_gen_new(Snap* snap, SCodeGen* up) {
  SCodeGen* c = (SCodeGen*)gc_new(snap,  STYPE_CODE_GEN, sizeof(SCodeGen));
  list_init(&c->insts);
  c->insts_count = 0;
  c->scope = NULL;
  snap_hash_init(&c->constants);
  snap_hash_init(&c->global_names);
  snap_vec_init(&c->param_names);
  c->num_locals = 0;
  c->is_tail = false;
  c->up = NULL;
  return c;
}

void snap_define(Snap* snap, const char* name, SValue val) {
  SValue key = create_obj((SObject*)snap_sym_new(snap, name));
  snap_hash_put(&snap->globals, key, val);
}

void snap_def_cfunc(Snap* snap, const char* name, SCFunc cfunc) {
  snap_define(snap, name, create_cfunc(cfunc));
}

void snap_throw(Snap* snap, int code, const char* format, ...) {
  char msg[256];
  va_list args;
  va_start(args, format);
  vsnprintf(msg, sizeof(msg), format, args);
  va_end(args);
  snap->cause = snap_err_new(snap, code, msg);
  //longjmp(snap->trying->buf, 1);
}


// Stack layout
// [args][locals][stack...]

// During call
// 1) [args and locals][stack...][calling args][function]
// 2) [args and locals][stack...][args and locals][stack...]
// 3) [args and locals][stack...][return value]

static void push_frame(Snap* snap, SCode* code, SValue* stack_base) {
  SnapFrame* frame;
  if (snap->frames_top + 1 >= snap->frames_size) {
    snap->frames_size = snap->frames_size > 0 ? 2 * snap->frames_size : 4;
    snap->frames = (SnapFrame*)realloc(snap->frames, snap->frames_size * sizeof(SnapFrame));
  }
  frame = &snap->frames[snap->frames_top++];
  frame->code = code;
  frame->pc = code->insts;
  if (stack_base + code->max_stack_size >= snap->stack_end) {
    int i;
    uintptr_t d;
    // TODO: Grow more intellegently than this
    int stack_size = (snap->stack_end - snap->stack) + code->max_stack_size;
    SValue* stack_old = snap->stack;
    SValue* stack_new = snap->stack = (SValue*)realloc(snap->stack, stack_size);
    snap->stack_end = stack_new + stack_size;

    // Update stack pointers to the new stack
    for (i = 0; i < snap->frames_top; ++i) {
      SnapFrame* frame = &snap->frames[i];
      d = frame->stack_base - stack_old;
      frame->stack_base = stack_new + d;
      d = frame->stack_top - stack_old;
      frame->stack_top = stack_new + d;
    }
    d = stack_base - stack_old;
    stack_base = stack_new + d;
  }
  frame->stack_base = stack_base;
  frame->stack_top = stack_base + code->num_locals;
}

#define binop(name, iop, fop) \
  a = --stack; \
  r = b = stack - 1; \
  if (is_int_p(a) && is_int_p(b)) { \
    *r = iop(a, b); \
  } else if (is_float_p(a) && is_float_p(b)) { \
    *r = fop(a, b); \
  } else { \
    return create_nil(); \
  } \

#define lt_iop(a, b) create_bool(a->i < b->i)
#define lt_fop(a, b) create_bool(a->f < b->f)

#define gt_iop(a, b) create_bool(a->i > b->i)
#define gt_fop(a, b) create_bool(a->f > b->f)

#define le_iop(a, b) create_bool(a->i <= b->i)
#define le_fop(a, b) create_bool(a->f <= b->f)

#define ge_iop(a, b) create_bool(a->i >= b->i)
#define ge_fop(a, b) create_bool(a->f >= b->f)

#define eq_iop(a, b) create_bool(a->i == b->i)
#define eq_fop(a, b) create_bool(a->f == b->f)

#define add_iop(a, b) create_int(a->i + b->i)
#define add_fop(a, b) create_float(a->f + b->f)

#define sub_iop(a, b) create_int(a->i - b->i)
#define sub_fop(a, b) create_float(a->f - b->f)

#define mul_iop(a, b) create_int(a->i * b->i)
#define mul_fop(a, b) create_float(a->f * b->f)

#define div_iop(a, b) create_int(a->i / b->i)
#define div_fop(a, b) create_float(a->f / b->f)

#define mod_iop(a, b) create_int(a->i % b->i)
#define mod_fop(a, b) create_float(fmod(a->f, b->f))

#define dispatch(n) pc += n; continue

static SValue exec(Snap* snap) {
  SnapFrame* frame;
  SCode* code;
  int* pc;
  SValue* constants;
  SValue* global_names;
  SValue* locals;
  SValue* stack;
  SValue* a, * b, * r;

new_frame:
    frame = &snap->frames[snap->frames_top - 1];
    code = frame->code;
    pc = frame->pc;
    constants = code->constants->data;
    global_names = code->global_names->data;
    locals = frame->stack_base;
    stack = frame->stack_top;

  for (;;) {
    int inst = *pc;
    int opcode = (inst >> 26) & 0x3F;
    int arg;
    switch (opcode) {
      case LOAD_GLOBAL:
        arg = inst & 0x3FFFFFF;
        a = snap_hash_get(&snap->globals, global_names[arg]);
        if (!a) {
          return create_nil();
        }
        *stack++ = *a;
        dispatch(1);
      case STORE_GLOBAL:
        arg = inst & 0x3FFFFFF;
        snap_hash_put(&snap->globals, global_names[arg], *--stack);
        dispatch(1);
      case LOAD_LOCAL:
        arg = inst & 0x3FFFFFF;
        *stack++ = locals[arg];
        dispatch(1);
      case STORE_LOCAL:
        arg = inst & 0x3FFFFFF;
        locals[arg] = *--stack;
        dispatch(1);
      case LOAD_CONSTANT:
        arg = inst & 0x3FFFFFF;
        *stack++ = constants[arg];
        dispatch(1);
      case LOAD_NIL:
        arg = inst & 0x3FFFFFF;
        *stack++ = create_nil();
        dispatch(1);
      case POP:
        --stack;
        dispatch(1);
      case CALL:
        arg = inst & 0x3FFFFFF;
        a = --stack; // Pop callable
        if (is_cfunc_p(a)) {
          SValue t = a->c(snap, stack - arg, arg);
          // TODO: Handle error
          stack -= arg; // Pop arguments
          *stack++ = t;
        } else if (is_code_p(a)){
          frame->pc = pc + 1;
          frame->stack_top = stack - arg; // Pop arguments
          push_frame(snap, (SCode*)a->o, frame->stack_top);
          goto new_frame;
        } else {
          // TODO: Handle code or error
          return create_nil();
        }
        dispatch(1);
      case RETURN:
        if (snap->frames_top > 1) {
          SnapFrame* frame_prev = &snap->frames[snap->frames_top - 2];
          *frame_prev->stack_top++ = *--stack;
          --snap->frames_top;
          goto new_frame;
        }
        return *--stack;
      case JUMP:
        arg = (inst & 0x3FFFFFF) - MAX_OP_ARG;
        dispatch(arg);
      case JUMP_FALSE:
        a = --stack;
        if (!is_bool_p(a) || !a->b) {
          arg = (inst & 0x3FFFFFF) - MAX_OP_ARG;
          dispatch(arg);
        }
        dispatch(1);
      case ADD: binop(add, add_iop, add_fop) dispatch(1);
      case SUB: binop(sub, sub_iop, sub_fop) dispatch(1);
      case MUL: binop(mul, mul_iop, mul_fop) dispatch(1);
      case DIV: binop(div, div_iop, div_fop) dispatch(1);
      case MOD: binop(mod, mod_iop, mod_fop) dispatch(1);
      case LESS_THAN: binop(lt, lt_iop, lt_fop) dispatch(1);
      case GREATER_THAN: binop(gt, gt_iop, gt_fop) dispatch(1);
      case LESS_EQUALS: binop(le, le_iop, le_fop) dispatch(1);
      case GREATER_EQUALS: binop(ge, ge_iop, ge_fop) dispatch(1);
      case EQUALS: binop(eq, eq_iop, eq_fop) dispatch(1);
    }
  }

  return create_nil();
}

SValue snap_exec(Snap* snap, const char* expr) {
  SnapLex lex;
  lex.buf = expr;
  lex.buf_size = strlen(lex.buf);
  lex.p = lex.buf;
  lex.line = 0;
  SCode* code = parse(snap, &lex);
  print_code(code, 0);
  push_frame(snap, code, snap->stack);
  return exec(snap);
}

void snap_parse(Snap* snap, const char* expr) {
  SnapLex lex;
  lex.buf = expr;
  lex.buf_size = strlen(lex.buf);
  lex.p = lex.buf;
  lex.line = 0;
  parse(snap, &lex);
}

SObject* snap_anchor(Snap* snap, SObject* obj) {
  if (snap->anchored_top >= snap->anchored_capacity) {
    snap->anchored_capacity *= 2;
    snap->anchored
        = (SObject**)realloc(snap->anchored,
                             snap->anchored_capacity * sizeof(SObject*));
  }
  snap->anchored[snap->anchored_top++] = obj;
  return obj;
}

void snap_release(Snap* snap) {
  assert(snap->anchored_top > 0);
  snap->anchored_top--;
}

static bool is_tail(SnapLex* lex, int token) {
  int count = (token == '(' ? 1 : 0);
  const char* p = lex->p;
  do {
    token = snap_lex_next_token(lex);
    if (token == '(') count++;
    if (token == ')') count--;
  } while (count > 0);
  lex->p = p;
  return token == ')';
}

static void parse_expr(Snap* snap, SnapLex* lex, SCodeGen* code_gen, int token);
static void parse_expr_list(Snap* snap, SnapLex* lex, SCodeGen* code_gen);

static SCode* parse(Snap* snap, SnapLex* lex) {
  int token;
  bool first = true;
  // TODO: Anchor
  SCodeGen* code_gen = snap_code_gen_new(snap, NULL);
  while ((token = snap_lex_next_token(lex)) != TK_EOF) {
    if (!first) insts_append(snap, code_gen, POP);
    parse_expr(snap, lex, code_gen, token);
    first = false;
  }
  insts_append(snap, code_gen, RETURN);
  return snap_code_new(snap, code_gen);
}

static int get_or_add_index(SnapHash* hash, SValue key) {
  int index;
  SValue* val = snap_hash_get(hash, key);
  if (val) {
    index = val->i;
  } else {
    index = hash->count;
    snap_hash_put(hash, key, create_int(index));
  }
  return index;
}

int add_local(Snap* snap, SCodeGen* code_gen, SSymStr* sym) {
  SScope* scope = code_gen->scope;
  SScope* up = scope;
  SValue key = create_obj((SObject*)sym);
  int index = 0;
  if (snap_hash_get(&code_gen->scope->local_names, key)) {
    snap_throw(snap, 0, "Variable '%s' already defined in current scope", sym->data);
  }
  while (up) {
    index += scope->local_names.count;
    up = scope->up;
  }
  if (index + 1 > code_gen->num_locals) {
    code_gen->num_locals = index + 1;
  }
  snap_hash_put(&scope->local_names, key, create_int(index));
  return index;
}

int add_global(SCodeGen* code_gen, SSymStr* sym) {
  SValue key = create_obj((SObject*)sym);
  SValue* val = snap_hash_get(&code_gen->global_names, key);
  if (val)  return val->i;
  int index = code_gen->global_names.count;
  snap_hash_put(&code_gen->global_names, key, create_int(index));
  return index;
}

static void load_constant(Snap* snap, SCodeGen* code_gen, SValue constant) {
  int index = get_or_add_index(&code_gen->constants, constant);
  insts_append(snap, code_gen, LOAD_CONSTANT)->arg = index;
}

static void load_variable(Snap* snap, SCodeGen* code_gen, SSymStr* sym) {
  SScope* scope = code_gen->scope;
  SValue key = create_obj((SObject*)sym);
  while (scope) {
    SValue* val = snap_hash_get(&scope->local_names, key);
    if (val) {
      insts_append(snap, code_gen, LOAD_LOCAL)->arg = val->i;
      return;
    }
    scope = scope->up;
  }
  {
    int index = get_or_add_index(&code_gen->global_names, key);
    insts_append(snap, code_gen, LOAD_GLOBAL)->arg = index;
  }
}

static void parse_define(Snap* snap, SnapLex* lex, SCodeGen* code_gen) {
  SSymStr* sym;
  int token = snap_lex_next_token(lex);
  if (token != TK_ID) {
    snap_throw(snap, 0, "Expected id for first argument of define at %d", lex->line);
  }
  sym = push_sym(snap_sym_new(snap, lex->val));
  parse_expr(snap, lex, code_gen, snap_lex_next_token(lex));
  if (code_gen->scope) {
    insts_append(snap, code_gen, STORE_LOCAL)->arg = add_local(snap, code_gen, sym);
  } else {
    insts_append(snap, code_gen, STORE_GLOBAL)->arg = add_global(code_gen, sym);
  }
  insts_append(snap, code_gen, LOAD_NIL); /* TODO: Hack */
  snap_release(snap);
  token = snap_lex_next_token(lex);
  if (token != ')') {
    snap_throw(snap, 0, "Expected ')' to terminate s-expression at %d", lex->line);
  }
}

static void parse_arith_expr(Snap* snap, SnapLex* lex, SCodeGen* code_gen, int opcode) {
  int i;
  int token;
  int count = 0;
  while ((token = snap_lex_next_token(lex)) != ')') {
    parse_expr(snap, lex, code_gen, token);
    count++;
  }
  if (count < 2) {
    snap_throw(snap, 0, "Arithmetic expression requires at least two arguments at %d", lex->line);
  }
  for (i = 1; i < count; ++i) {
    insts_append(snap, code_gen, opcode);
  }
}

static void parse_call(Snap* snap, SnapLex* lex, SCodeGen* code_gen) {
  int token;
  int count = 0;
  SSymStr* sym = push_sym(snap_sym_new(snap, lex->val));
  while ((token = snap_lex_next_token(lex)) != ')') {
    parse_expr(snap, lex, code_gen, token);
    count++;
  }
  load_variable(snap, code_gen, sym);
  insts_append(snap, code_gen, CALL)->arg = count;
  snap_release(snap);
}

static void parse_if(Snap* snap, SnapLex* lex, SCodeGen* code_gen) {
  int token;
  SValue jump_val;
  SInst* cond_jump, * jump;
  parse_expr(snap, lex, code_gen, snap_lex_next_token(lex));
  cond_jump = insts_append(snap, code_gen, JUMP_FALSE);
  // true
  parse_expr(snap, lex, code_gen, snap_lex_next_token(lex));
  jump = insts_append(snap, code_gen, JUMP);
  jump_arg_init(&cond_jump->jump_arg, 1, code_gen->insts.prev);
  //cond_jump->arg = insts_forward_count(cond_jump->list.next, (SnapNode*)&code_gen->insts) + 1;
  // false
  parse_expr(snap, lex, code_gen, snap_lex_next_token(lex));
  jump_arg_init(&jump->jump_arg, 1, code_gen->insts.prev);
  //jump->arg = insts_forward_count(jump->list.next, (SnapNode*)&code_gen->insts) + 1;
  token = snap_lex_next_token(lex);
  if (token != ')') {
    snap_throw(snap, 0, "Expected ')' to terminate s-expression at %d", lex->line);
  }
}

static void parse_quote(Snap* snap, SnapLex* lex, SCodeGen* code_gen) {
}

static void parse_set(Snap* snap, SnapLex* lex, SCodeGen* code_gen) {
}

static void parse_fn(Snap* snap, SnapLex* lex, SCodeGen* up) {
  // TODO: Anchor
  int token;
  SCodeGen* code_gen = snap_code_gen_new(snap, up);
  code_gen->scope = snap_scope_new(snap, NULL);
  token = snap_lex_next_token(lex);
  if (token != '(') {
    snap_throw(snap, 0, "Expected parameter list at %d", lex->line);
  }
  token = snap_lex_next_token(lex);
  while (token == TK_ID) {
    SSymStr* sym = snap_sym_new(snap, lex->val);
    snap_vec_push(&code_gen->param_names, create_obj((SObject*)sym));
    add_local(snap, code_gen, sym);
    token = snap_lex_next_token(lex);
  }
  if (token != ')') {
    snap_throw(snap, 0, "Expected ')' to terminate s-expression at %d", lex->line);
  }
  parse_expr_list(snap, lex, code_gen);
  insts_append(snap, code_gen, RETURN);
  load_constant(snap, up, create_obj((SObject*)snap_code_new(snap, code_gen)));
}

static void parse_let(Snap* snap, SnapLex* lex, SCodeGen* code_gen) {
  // New scope
}

static void parse_recur(Snap* snap, SnapLex* lex, SCodeGen* code_gen) {
  int i;
  int token;
  int count = 0;
  SnapVec* param_names = &code_gen->param_names;
  while ((token = snap_lex_next_token(lex)) != ')') {
    parse_expr(snap, lex, code_gen, token);
    count++;
  }
  if (count != param_names->size) {
    snap_throw(snap, 0, "Invalid number of arguments", lex->line);
  }
  for (i = 0; i < count; ++i) {
    int index = get_or_add_index(&code_gen->scope->local_names, param_names->items[i]);
    insts_append(snap, code_gen, STORE_LOCAL)->arg =  index;
  }
  jump_arg_init(&insts_append(snap, code_gen, JUMP)->jump_arg, -1, code_gen->insts.next);
}

static void parse_sexpr(Snap* snap, SnapLex* lex, SCodeGen* code_gen) {
  int token = snap_lex_next_token(lex);
  switch (token) {
    case ')':
      break;
    case TK_ID:
      parse_call(snap, lex, code_gen);
      break;
    case TK_DO:
      parse_expr_list(snap, lex, code_gen);
      break;
    case TK_DEF:
      parse_define(snap, lex, code_gen);
      break;
    case TK_IF:
      parse_if(snap, lex, code_gen);
      break;
    case TK_FN:
      parse_fn(snap, lex, code_gen);
      break;
    case TK_LET:
      parse_let(snap, lex, code_gen);
      break;
    case TK_QUOTE:
      break;
    case TK_RECUR:
      parse_recur(snap, lex, code_gen);
      break;
    case TK_SET:
      break;
    case TK_THROW:
      break;
    case TK_TRY:
      break;
    case TK_ADD: case TK_SUB: case TK_MUL: case TK_DIV: case TK_MOD:
      parse_arith_expr(snap, lex, code_gen, (token - TK_ADD) + ADD);
      break;
    default:
      snap_throw(snap, 0, "Expected id, special form or expr at %d", lex->line);
      break;
  }
}

static void parse_expr(Snap* snap, SnapLex* lex, SCodeGen* code_gen, int token) {
  switch (token) {
    case '(':
      parse_sexpr(snap, lex, code_gen);
      break;
    case '\'':
      break;
    case TK_INT:
      load_constant(snap, code_gen, create_int(atol(lex->val)));
      break;
    case TK_FLOAT:
      load_constant(snap, code_gen, create_float(atof(lex->val)));
      break;
    case TK_STR:
      load_constant(snap, code_gen, create_obj((SObject*)snap_str_new(snap, lex->val)));
      break;
    case TK_ID:
      load_variable(snap, code_gen, push_sym(snap_sym_new(snap, lex->val)));
      snap_release(snap);
      break;
    case TK_TRUE:
    case TK_FALSE:
      load_constant(snap, code_gen, create_bool(token == TK_TRUE));
      break;
    case TK_NIL:
      insts_append(snap, code_gen, LOAD_NIL);
      break;
    case TK_DO:
    case TK_DEF:
    case TK_ELLIPSIS:
    case TK_IF:
    case TK_FN:
    case TK_LET:
    case TK_QUOTE:
    case TK_RECUR:
    case TK_SET:
    case TK_THROW:
    case TK_TRY:
      // error
      break;
    case TK_EOF:
      break;
    default:
      snap_throw(snap, 0, "Invalid token on line %d", lex->line);
      break;
  }
}

static void parse_expr_list(Snap* snap, SnapLex* lex, SCodeGen* code_gen) {
  int token;
  int count = 0;
  bool first = true;
  while ((token = snap_lex_next_token(lex)) != ')') {
    if (!first) {
      insts_append(snap, code_gen, POP);
    }
    first = false;
    parse_expr(snap, lex, code_gen, token);
    count++;
  }
  if (count < 1) {
    snap_throw(snap, 0, "Expression exepcted at %d", lex->line);
  }
}

static inline void list_init(SnapNode* list) {
  list->next = list->prev = list;
}

static inline bool list_is_empty(SnapNode* list) {
  return list->next == list;
}

static inline void list_add(SnapNode* prev, SnapNode* next, SnapNode* entry) {
  prev->next = entry;
  entry->prev = prev;
  entry->next = next;
  next->prev = entry;
}

static void list_remove(SnapNode* node) {
  node->prev->next = node->next;
  node->next->prev = node->prev;
  node->next = NULL;
  node->prev = NULL;
}

static inline void list_append(SnapNode* list, SnapNode* node) {
  list_add(list->prev, list, node);
}

static inline void list_prepend(SnapNode* list, SnapNode* node) {
  list_add(list, list->next , node);
}

static int list_forward_count(SnapNode* first, SnapNode* last) {
  int count = 0;
  SnapNode* curr = first;
  while (curr != last) {
    count++;
    curr = curr->next;
  }
  return count++;
}

static int list_backward_count(SnapNode* first, SnapNode* last) {
  int count = 0;
  SnapNode* curr = first;
  while (curr != last) {
    count++;
    curr = curr->prev;
  }
  return count++;
}

static void code_gen_destroy(SCodeGen* code_gen) {
  snap_hash_destroy(&code_gen->constants);
  snap_hash_destroy(&code_gen->global_names);
  free(code_gen);
}

static SInst* insts_append(Snap* snap, SCodeGen* code_gen, int opcode) {
  SInst* inst = snap_inst_new(snap, opcode);
  code_gen->insts_count++;
  list_append(&code_gen->insts, &inst->list);
  return inst;
}

void snap_print(SValue val) {
  print_val(val, 0);
}

static void iprintf(int indent, const char* format, ...) {
  va_list args;
  va_start(args, format);
  printf("%*s", indent, "");
  vprintf(format, args);
  va_end(args);
}

static void print_val(SValue val, int indent) {
  switch(val.type) {
    case STYPE_NIL:
      iprintf(indent, "nil");
      break;
    case STYPE_BOOL:
      iprintf(indent, "%s", val.b ? "true" : "false");
      break;
    case STYPE_INT:
      iprintf(indent, "%ld", val.i);
      break;
    case STYPE_FLOAT:
      iprintf(indent, "%f", val.f);
      break;
    case STYPE_CFUNC:
      iprintf(indent, "<cfunc> %p", val.c);
      break;
    case STYPE_FORM:
      switch (val.i) {
        case TK_DO: iprintf(indent, "do"); break;
        case TK_DEF: iprintf(indent, "def"); break;
        case TK_ELLIPSIS: iprintf(indent, "..."); break;
        case TK_IF: iprintf(indent, "if"); break;
        case TK_FN: iprintf(indent, "fn"); break;
        case TK_LET: iprintf(indent, "let"); break;
        case TK_QUOTE: iprintf(indent, "quote"); break;
        case TK_RECUR: iprintf(indent, "recur"); break;
        case TK_SET: iprintf(indent, "set"); break;
        case TK_THROW: iprintf(indent, "throw"); break;
        case TK_TRY: iprintf(indent, "try"); break;
      }
      break;
    case STYPE_STR:
      iprintf(indent, "\"%s\"", as_str(val)->data);
      break;
    case STYPE_SYM:
      iprintf(indent, "%s", as_sym(val)->data);
      break;
    case STYPE_ERR:
      iprintf(indent, "<err> %d '%s'", as_err(val)->code, as_err(val)->msg->data);
      break;
    case STYPE_CONS:
      iprintf(indent, "(");
      print_cons(as_cons(val), indent);
      iprintf(indent, ")");
      break;
    case STYPE_CODE:
      iprintf(indent, "<code object (%p)>:\n", val.o);
      print_code(as_code(val), indent + 1);
      break;
  }
}

static void print_cons(SCons* cons, int indent) {
  int space = 0;
  while (cons != NULL) {
    if (space) printf(" ");
    space = 1;
    print_val(cons->first, indent);
    if (is_cons(cons->rest)) {
      cons = as_cons(cons->rest);
    } else {
      printf(" . ");
      print_val(cons->rest, indent);
      break;
    }
  }
}

static void print_code(SCode* code, int indent) {
  int i = 0;
  if (code->global_names->len > 0) {
    iprintf(indent, "global names: {\n");
    for (i = 0; i < code->global_names->len; ++i) {
      iprintf(indent + 1, "%d => ", i);
      print_val(code->global_names->data[i], 0);
      printf("\n");
    }
    iprintf(indent, "}\n");
  }
  if (code->constants->len > 0) {
    iprintf(indent, "constants: {\n");
    for (i = 0; i < code->constants->len; ++i) {
      if (is_code(code->constants->data[i])) {
        iprintf(indent + 1, "%d =>\n", i);
        print_val(code->constants->data[i], indent + 2);
      } else {
        iprintf(indent + 1, "%d => ", i);
        print_val(code->constants->data[i], 0);
        printf("\n");
      }
    }
    iprintf(indent, "}\n");
  }
  iprintf(indent, "instructions: {\n");
  for (i = 0; i < code->insts_count; ++i) {
    int inst = code->insts[i];
    int op = (inst >> 26) & 0x3F;
    switch (op) {
#define XX(opcode, name, has_arg) \
      case opcode: \
        if (!has_arg) { iprintf(indent + 1, "%04d: %s\n", i, name); } \
        else { iprintf(indent + 1, "%04d: %s %d\n", i, name, \
                      op == JUMP || op == JUMP_FALSE \
                      ? (inst & 0x3FFFFFF) - MAX_OP_ARG : inst & 0x3FFFFFF); } \
        break;
      OPCODE_MAP(XX)
#undef XX
    }
  }
  iprintf(indent, "}\n");
}

static int insts_calculate_jump(SInst* inst) {
  int total = 0;
  SInst* prev_inst;
  do {
    int i;
    prev_inst = inst;
    if (inst->jump_arg.dir > 0) {
      int jump = list_forward_count(&inst->list, inst->jump_arg.dest) + 1;
      for (i = 0; i < jump; ++i) {
        inst = list_entry(SInst, inst->list.next, list);
      }
      total += jump;
    } else {
      int jump = list_backward_count(&inst->list, inst->jump_arg.dest);
      for (i = 0; i < jump; ++i) {
        inst = list_entry(SInst, inst->list.prev, list);
      }
      total -= jump;
    }
  } while (inst->opcode == JUMP && prev_inst != inst);
  return total;
}

static void insts_remove(SnapNode* insts, SInst* to_remove) {
  SnapNode* pos;
  list_foreach(insts) {
    SInst* inst = list_entry(SInst, pos, list);
    switch (inst->opcode) {
      case JUMP:
      case JUMP_FALSE:
        if (inst->jump_arg.dest == &to_remove->list) {
          inst->jump_arg.dest = to_remove->list.next;
        }
        break;
    }
  }
  list_remove(&to_remove->list);
}

static void insts_remove_dead_code(SnapNode* insts) {
  SnapNode* pos = insts->next;
  while (pos != insts) {
    SInst* inst = list_entry(SInst, pos, list);
    SInst* prev_inst;
    pos = pos->next;
    switch (inst->opcode) {
      case POP:
        assert(inst->list.prev != insts);
        prev_inst = list_entry(SInst, inst->list.prev, list);
        switch (prev_inst->opcode) {
          case LOAD_CONSTANT:
          case LOAD_GLOBAL:
          case LOAD_LOCAL:
          case LOAD_NIL:
            insts_remove(insts, prev_inst);
            insts_remove(insts, inst);
            break;
        }
        break;
      case STORE_GLOBAL:
        assert(inst->list.prev != insts);
        prev_inst = list_entry(SInst, inst->list.prev, list);
        if (prev_inst->opcode == LOAD_GLOBAL && prev_inst->arg == inst->arg) {
          insts_remove(insts, prev_inst);
          insts_remove(insts, inst);
        }
        break;
      case STORE_LOCAL:
        assert(inst->list.prev != insts);
        prev_inst = list_entry(SInst, inst->list.prev, list);
        if (prev_inst->opcode == LOAD_LOCAL && prev_inst->arg == inst->arg) {
          insts_remove(insts, prev_inst);
          insts_remove(insts, inst);
        }
        break;
    }
  }
}

static void insts_dump(SnapNode* insts) {
  SnapNode* pos;
  int i = 0;
  list_foreach(insts) {
    SInst* inst = list_entry(SInst, pos, list);
    switch(inst->opcode) {
#define XX(opcode, name, has_arg) \
      case opcode: \
        if (!has_arg) { printf("%04d: %s\n", i, name); } \
        else { printf("%04d: %s %d\n", i, name, \
                      inst->arg == JUMP || inst->arg == JUMP_FALSE \
                      ? inst->arg - MAX_OP_ARG : inst->arg); } \
        break;
      OPCODE_MAP(XX)
    #undef XX
    }
    i++;
  }
}

SValue builtin_print(Snap* snap, const SValue* args, int num_args) {
  if (num_args > 0) {
    snap_print(args[0]);
  }
  return create_nil();
}

void snap_init(Snap* snap) {
  snap->stack = (SValue*)malloc(1024 * sizeof(SValue));
  snap->stack_end = snap->stack + 1024 * sizeof(SValue);
  snap->frames = NULL;
  snap->frames_top = 0;
  snap->frames_size = 0;
  snap->anchored = (SObject**)malloc(32 * sizeof(SObject*));
  snap->anchored_capacity = 32;
  snap->anchored_top = 0;
  snap->num_bytes_alloced = 0;
  snap->num_bytes_alloced_last_gc = 0;
  snap->all = NULL;
  snap->gray = NULL;
  snap->cause = NULL;
  snap_hash_init(&snap->globals);

  snap_define(snap, "print", create_cfunc(builtin_print));
}

void snap_destroy(Snap* snap) {
  SObject* obj = snap->all;
  while (obj) {
    SObject* temp = obj;
    obj = obj->gc_next;
    gc_free(snap, temp);
  }
  free((void*)snap->stack);
  free((void*)snap->anchored);
  snap_hash_destroy(&snap->globals);
  assert(snap->num_bytes_alloced == 0);
}
