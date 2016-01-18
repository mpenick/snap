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
  XX(DUP, "DUP", false) \
  XX(ROT2, "ROT2", false) \
  XX(ROT3, "ROT3", false) \
  XX(CALL, "CALL", true) \
  XX(RETURN, "RETURN", false) \
  XX(JUMP, "JUMP", true) \
  XX(JUMP_FALSE, "JUMP_FALSE", true) \
  XX(JUMP_FALSE_OR_POP, "JUMP_FALSE_OR_POP", true) \
  XX(BLOCK, "BLOCK", true) \
  XX(END_BLOCK, "END_BLOCK", false) \
  XX(RAISE, "RAISE", false) \
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

#define anchor(type, obj) (type*)snap_anchor(snap, (SObject*)obj)
#define anchor_sym(sym) anchor(SSymStr, sym)
#define anchor_str(str) anchor(SSymStr, str)
#define anchor_key(key) anchor(SKeyword, key)
#define anchor_cons(cons) anchor(SCons, cons)
#define anchor_code(code) anchor(SCode, code)
#define anchor_code_gen(code_gen) anchor(SCodeGen, code_gen)

static SValue exec(Snap* snap);
static void compile(Snap* snap, SCodeGen* code_gen, SValue value);
static bool parse(Snap* snap, SnapLex* lex, SValue *result);

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
static void print_arr(SArr* arr, int indent);

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

SValue create_form(SnapInt i) {
  SValue val;
  val.type = STYPE_FORM;
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

SValue create_object(SObject* o) {
  SValue val;
  val.type = o->type;
  val.o = o;
  return val;
}

#define create_obj(o) create_object((SObject*)o)

static void gc_free(Snap* snap, SObject* obj) {
  //printf("free %p\n", obj);
  switch (obj->type) {
    case STYPE_SYM:
    case STYPE_STR:
      snap->num_bytes_alloced -= (sizeof(SSymStr) + ((SSymStr*)obj)->len + 1);
      break;
    case STYPE_KEY:
      snap->num_bytes_alloced -= (sizeof(SKeyword) + ((SKeyword*)obj)->len + 1);
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
    case STYPE_KEY:
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
      gc_mark(snap, (SObject*)((SErr*)obj)->err);
      gc_mark_val(snap, ((SErr*)obj)->msg);
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
  gc_mark_hash(snap, &snap->keywords);
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

SKeyword* snap_key_new(Snap* snap, const char* name) {
  size_t len = strlen(name);
  SValue* existing = snap_hash_get_str(&snap->keywords, name, len);
  if (existing) {
    return (SKeyword*)existing->o;
  } else  {
    SKeyword* k = (SKeyword*)gc_new(snap, STYPE_KEY, sizeof(SKeyword) + len + 1);
    k->len = len;
    strcpy(k->data, name);
    k->id = snap->keywords.count;
    snap_hash_put(&snap->keywords, create_obj(k), create_obj(k));
    return k;
  }
}

SErr* snap_err_new(Snap* snap, SKeyword* err, SValue msg) {
  SErr* e = (SErr*)gc_new(snap, STYPE_ERR, sizeof(SErr));
  e->err = err;
  e->msg = msg;
  return e;
}

SErr* snap_err_new_str(Snap* snap, const char* err, const char* msg) {
  SSymStr* s = anchor_str(snap_str_new(snap, msg));
  SKeyword* k = snap_key_new(snap, err);
  SErr* e = snap_err_new(snap, k, create_obj(s));
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

  //insts_dump(&code_gen->insts);
  printf("\n");
  insts_remove_dead_code(&code_gen->insts);
  //insts_dump(&code_gen->insts);
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
        case DUP:
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
        case JUMP_FALSE_OR_POP:
          inst->arg = insts_calculate_jump(inst) + MAX_OP_ARG;
          break;
        case BLOCK:
          inst->arg = insts_calculate_jump(inst) + MAX_OP_ARG;
          stack_size--;
          break;
        case END_BLOCK:
          stack_size--;
          break;
        case RAISE:
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
  SValue key = create_obj(snap_sym_new(snap, name));
  snap_hash_put(&snap->globals, key, val);
}

void snap_def_cfunc(Snap* snap, const char* name, SCFunc cfunc) {
  snap_define(snap, name, create_cfunc(cfunc));
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
  frame->blocks_top = 0;
  if (stack_base + code->max_stack_size >= snap->stack_end) {
    int i;
    uintptr_t d;
    // TODO: Grow more intelligently than this
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

#define binary_op(name, iop, fop) \
  b = --stack; \
  r = a = stack - 1; \
  if (is_int_p(a) && is_int_p(b)) { \
    *r = iop(a, b); \
  } else if (is_float_p(a) && is_float_p(b)) { \
    *r = fop(a, b); \
  } else { \
    return create_nil(); \
  }

#define compare_op(op) \
  b = --stack; \
  r = a = stack - 1; \
  if (is_int_p(a) && is_int_p(b)) { \
    *r = create_bool(a->i op b->i); \
  } else if (is_float_p(a) && is_float_p(b)) { \
    *r = create_bool(a->f op b->f); \
  } else { \
    *r = create_bool(snap_compare(a, b) op 0); \
  }

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
  SnapBlock* block;
  SCode* code;
  int* pc;
  SValue* constants;
  SValue* global_names;
  SValue* locals;
  SValue* stack;
  SValue* a, * b, * r;
  SValue temp;
  int i, arg;

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
      case DUP:
        *stack = *(stack - 1);
        stack++;
        dispatch(1);
      case ROT2:
        temp = *(stack - 1);
        *(stack - 1) = *(stack - 2);
        *(stack - 2) = temp;
        dispatch(1);
      case ROT3:
        temp = *(stack - 3);
        *(stack - 3) = *(stack - 1);
        *(stack - 1) = *(stack - 2);
        *(stack - 2) = temp;
        dispatch(1);
      case CALL:
        arg = inst & 0x3FFFFFF;
        a = --stack; // Pop callable
        if (is_cfunc_p(a)) {
          temp = create_nil();
          frame->stack_top = stack - arg;
          a->c(snap, stack - arg, arg, &temp);
          stack -= arg; // Pop arguments
          *stack++ = temp;
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
      case JUMP_FALSE_OR_POP:
        a = stack - 1;
        if (!is_bool_p(a) || !a->b) {
          arg = (inst & 0x3FFFFFF) - MAX_OP_ARG;
          dispatch(arg);
        }
        --stack;
        dispatch(1);
      case BLOCK:
        // TODO: Handle too many blocks
        arg = (inst & 0x3FFFFFF) - MAX_OP_ARG;
        block = &frame->blocks[frame->blocks_top++];
        block->insts_offset = (pc - code->insts) + arg;
        block->stack_offset = stack - snap->stack;
        dispatch(1);
      case END_BLOCK:
        --frame->blocks_top;
        dispatch(1);
      case RAISE:
        goto raise;
        dispatch(1);
      case ADD: binary_op(add, add_iop, add_fop) dispatch(1);
      case SUB: binary_op(sub, sub_iop, sub_fop) dispatch(1);
      case MUL: binary_op(mul, mul_iop, mul_fop) dispatch(1);
      case DIV: binary_op(div, div_iop, div_fop) dispatch(1);
      case MOD: binary_op(mod, mod_iop, mod_fop) dispatch(1);
      case LESS_THAN: compare_op(<) dispatch(1);
      case GREATER_THAN: compare_op(>) dispatch(1);
      case LESS_EQUALS: compare_op(<=) dispatch(1);
      case GREATER_EQUALS: compare_op(>=) dispatch(1);
      case EQUALS: compare_op(==) dispatch(1);
    }
  }

  return create_nil();

raise:
  while (snap->frames_top > 0) {
    frame = &snap->frames[snap->frames_top - 1];
    if (frame->blocks_top > 0) break;
    --snap->frames_top;
  }

  if (frame->blocks_top > 0) {
    block = &frame->blocks[--frame->blocks_top];
    frame->pc = frame->code->insts + block->insts_offset;
    frame->stack_top = snap->stack + block->stack_offset;
    *frame->stack_top++ = *(stack - 2);
    *frame->stack_top++ = *(stack - 1);
    goto new_frame;
  }

  return create_obj(snap_err_new(snap,
                                 (SKeyword*)(stack - 1)->o,
                                 *(stack - 2)));
}

SValue snap_exec(Snap* snap, const char* str) {
  SnapLex lex;
  SValue expr, result;
  SCodeGen* code_gen;
  SCode* code;
  bool first = true;

  lex.buf = str;
  lex.buf_size = strlen(lex.buf);
  lex.p = lex.buf;
  lex.line = 1;

  code_gen = anchor_code_gen(snap_code_gen_new(snap, NULL));
  while (parse(snap, &lex, &expr)) {
    snap_print(expr);
    printf("\n");
    if (!first)  insts_append(snap, code_gen, POP);
    first = false;
    compile(snap, code_gen, expr);
  }
  insts_append(snap, code_gen, RETURN);

  if (expr.type == STYPE_ERR) {
    result = expr;
    goto err;
  }

  code = anchor_code(snap_code_new(snap, code_gen));
  print_code(code, 0);
  push_frame(snap, code, snap->stack);
  result = exec(snap);
  snap_release(snap);

err:
  snap_release(snap);
  return result;
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

static void compile_expr(Snap* snap, SnapLex* lex, SCodeGen* code_gen, int token);
static int compile_expr_list(Snap* snap, SCodeGen* code_gen, SCons* sexpr);
static void compile_sexpr(Snap* snap, SCodeGen* code_gen, SCons* sexpr);

static void raise(Snap* snap, const char* format, ...) {
  char msg[256];
  va_list args;
  va_start(args, format);
  vsnprintf(msg, sizeof(msg), format, args);
  va_end(args);
  snap->cause = snap_err_new_str(snap, "parse_error", msg);
  longjmp(snap->jmp, 1);
}

static SValue parse_expr(Snap* snap, SnapLex* lex, int token);
static SValue parse_sexpr(Snap* snap, SnapLex* lex, int token);

static int parse_expr_list(Snap* snap, SnapLex* lex, SCons* sexpr) {
  int token;
  int count = 0;
  SCons** curr = (SCons**)&sexpr->rest.o;
  while ((token = snap_lex_next_token(lex)) != ')') {
    (*curr) = snap_cons_new(snap);
    (*curr)->first = parse_expr(snap, lex, token);
    curr = (SCons**)&(*curr)->rest.o;
    count++;
  }
  if (token != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }
  return count;
}

static int parse_cons(Snap* snap, SnapLex* lex, int token, SCons* sexpr) {
  sexpr->first = create_form(token);
  return parse_expr_list(snap, lex, sexpr);
}

static void parse_call(Snap* snap, SnapLex* lex, SCons* sexpr) {
  int token;
  sexpr->first = create_obj(snap_sym_new(snap, lex->val));
  parse_expr_list(snap, lex, sexpr);
}

static void parse_def(Snap* snap, SnapLex* lex, SCons* sexpr) {
  sexpr->first = create_form(TK_DEF);
  sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));
  if (snap_lex_next_token(lex) != TK_ID) {
    raise(snap, "Expected id for first argument of define at %d", lex->line);
  }
  sexpr->first = create_obj(snap_sym_new(snap, lex->val));
  sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));
  sexpr->first = parse_expr(snap, lex, snap_lex_next_token(lex));
  if (snap_lex_next_token(lex) != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }
}

static void parse_if(Snap* snap, SnapLex* lex, SCons* sexpr) {
  sexpr->first = create_form(TK_IF);
  sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));

  sexpr->first = parse_expr(snap, lex, snap_lex_next_token(lex));
  sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));

  /* true */
  sexpr->first = parse_expr(snap, lex, snap_lex_next_token(lex));
  sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));

  /* false */
  sexpr->first = parse_expr(snap, lex, snap_lex_next_token(lex));

  if (snap_lex_next_token(lex) != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }
}

static void parse_fn(Snap* snap, SnapLex* lex, SCons* sexpr) {
  int token;
  int i, len = 0;
  const char* p;
  SArr* arr;

  sexpr->first = create_form(TK_FN);
  sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));

  if (snap_lex_next_token(lex) != '[') {
    raise(snap, "Expected parameter list at %d", lex->line);
  }

  p = lex->p;
  while ((token = snap_lex_next_token(lex)) == TK_ID) {
    len++;
  }
  if (token != ']') {
    raise(snap, "Expected ']' to terminate parameter list at %d", lex->line);
  }
  lex->p = p;

  arr = snap_arr_new(snap, len);
  sexpr->first = create_obj(arr);

  for (i = 0; i < len; ++i) {
    snap_lex_next_token(lex);
    arr->data[i] = create_obj(snap_sym_new(snap, lex->val));
  }
  snap_lex_next_token(lex);

  parse_expr_list(snap, lex, sexpr);
}

static void parse_let(Snap* snap, SnapLex* lex, SCons* sexpr) {
}

static void parse_quote(Snap* snap, SnapLex* lex, SCons* sexpr) {
}

static void parse_set(Snap* snap, SnapLex* lex, SCons* sexpr) {
  sexpr->first = create_form(TK_SET);
  sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));
  if (snap_lex_next_token(lex) != TK_ID) {
    raise(snap, "Expected id for first argument of set! at %d", lex->line);
  }
  sexpr->first = create_obj(snap_sym_new(snap, lex->val));
  sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));
  sexpr->first = parse_expr(snap, lex, snap_lex_next_token(lex));
  if (snap_lex_next_token(lex) != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }
}

static void parse_raise(Snap* snap, SnapLex* lex, SCons* sexpr) {
  int token;

  sexpr->first = create_form(TK_RAISE);
  sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));

  if ((token = snap_lex_next_token(lex)) == TK_KEY) {
    sexpr->first = create_obj(snap_key_new(snap, lex->val));
  } else {
    sexpr->first = create_obj(snap_key_new(snap, "error"));
  }

  sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));

  if ((token = snap_lex_next_token(lex)) != ')') {
    sexpr->first = parse_expr(snap, lex, token);
  } else {
    sexpr->first = create_nil();
  }

  if (snap_lex_next_token(lex) != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }
}

static bool parse_catch(Snap* snap, SnapLex* lex, SCons* sexpr) {
  int token;
  int i, len = 0;
  const char* p;
  SArr* arr;
  bool final_catch = false;

  sexpr->first = create_form(TK_CATCH);
  sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));

  if ((token = snap_lex_next_token(lex)) == TK_KEY) {
    sexpr->first = create_obj(snap_key_new(snap, lex->val));
    sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));
    token = snap_lex_next_token(lex);
  } else {
    final_catch = true;
  }

  if (token != '[') {
    raise(snap, "Expected '[' to for catch arguments at %d", lex->line);
  }

  p = lex->p;
  while ((token = snap_lex_next_token(lex)) != ']') {
    if (token != TK_ID) {
      raise(snap, "Expected ids for catch arguments at %d", lex->line);
    }
    len++;
  }
  if (token != ']') {
    raise(snap, "Expected ']' to terminate catch arguments at %d", lex->line);
  }
  lex->p = p;

  if (len > 2) {
    raise(snap, "Invalid number of catch argument (0 - 2) %d", lex->line);
  }

  arr = snap_arr_new(snap, len);
  sexpr->first = create_obj(arr);

  for (i = 0; i < len; ++i) {
    snap_lex_next_token(lex);
    arr->data[i] = create_obj(snap_sym_new(snap, lex->val));
  }
  snap_lex_next_token(lex);

  parse_expr_list(snap, lex, sexpr);

  return final_catch;
}

static void parse_try(Snap* snap, SnapLex* lex, SCons* sexpr) {
  /* (try <exprs> ...
   *  (catch :err1 [<args> ...] <exprs> ...)
   *  (catch :err2 [<args> ...] <exprs> ...)
   *  ...
   *  (catch [<args> ...] <exprs> ...)
   * )
   */
  int token;
  int count = 0;
  SCons** curr = (SCons**)&sexpr->rest.o;

  sexpr->first = create_form(TK_TRY);

  while ((token = snap_lex_next_token(lex)) != ')') {
    if (token == TK_EOF) break;
    if (token == '(') {
      token = snap_lex_next_token(lex);
      if (token == TK_CATCH) {
        break;
      }
      (*curr) = snap_cons_new(snap);
      (*curr)->first = parse_sexpr(snap, lex, token);
      curr = (SCons**)&(*curr)->rest.o;
      count++;
    } else {
      (*curr) = snap_cons_new(snap);
      (*curr)->first = parse_expr(snap, lex, token);
      curr = (SCons**)&(*curr)->rest.o;
      count++;
    }
  }

  if (count < 1) {
    raise(snap, "Expression expected at %d", lex->line);
  }

  if (token != TK_CATCH) {
    raise(snap, "catch expected at %d", lex->line);
  }

  do {
    (*curr) = snap_cons_new(snap);
    sexpr = as_cons((*curr)->first = create_obj(snap_cons_new(snap)));
    if (parse_catch(snap, lex, sexpr)) {
      token = snap_lex_next_token(lex);
      break;
    } else if ((token = snap_lex_next_token(lex)) == ')') {
      break;
    }
    if (token != '(' || (token = snap_lex_next_token(lex)) != TK_CATCH) {
      raise(snap, "catch expected at %d", lex->line);
    }
    curr = (SCons**)&(*curr)->rest.o;
  } while(1);

  if (token != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }
}

static SValue parse_sexpr(Snap* snap, SnapLex* lex, int token) {
  SCons* sexpr = anchor_cons(snap_cons_new(snap));
  switch (token) {
    case ')': break;
    case TK_ID: parse_call(snap, lex, sexpr); break;
    case TK_DO: parse_cons(snap, lex, token, sexpr); break;
    case TK_DEF: parse_def(snap, lex, sexpr); break;
    case TK_IF: parse_if(snap, lex, sexpr); break;
    case TK_FN: parse_fn(snap, lex, sexpr); break;
    case TK_LET: parse_let(snap, lex, sexpr); break;
    case TK_QUOTE: parse_quote(snap, lex, sexpr); break;
    case TK_RECUR: parse_cons(snap, lex, token, sexpr); break;
    case TK_SET: parse_set(snap, lex, sexpr); break;
    case TK_RAISE: parse_raise(snap, lex, sexpr); break;
    case TK_TRY: parse_try(snap, lex, sexpr); break;
    case TK_ADD: case TK_SUB: case TK_MUL: case TK_DIV: case TK_MOD:
      if (parse_cons(snap, lex, token, sexpr) < 2) {
        raise(snap, "Arithmetic expression requires at least two arguments at %d", lex->line);
      }
      break;
    case TK_LT: case TK_LE: case TK_GT: case TK_GE: case TK_EQ:
      if (parse_cons(snap, lex, token, sexpr) < 2) {
        raise(snap, "Conditional expression requires at least two arguments at %d", lex->line);
      }
      break;
    default:
      raise(snap, "Expected id, special form or expr at %d", lex->line);
      break;
  }
  return create_obj(sexpr);
}

static SValue parse_expr(Snap* snap, SnapLex* lex, int token) {
  switch (token) {
    case '(':
      return parse_sexpr(snap, lex, snap_lex_next_token(lex));
    case '\'':
      break;
    case TK_INT:
      return create_int(atol(lex->val));
    case TK_FLOAT:
      return create_float(atof(lex->val));
    case TK_STR:
      return create_obj(snap_str_new(snap, lex->val));
    case TK_ID:
      return create_obj(snap_sym_new(snap, lex->val));
    case TK_KEY:
      return create_obj(snap_key_new(snap, lex->val));
    case TK_TRUE:
    case TK_FALSE:
      return create_bool(token == TK_TRUE);
    case TK_NIL:
      return create_nil();
    case TK_DO:
    case TK_DEF:
    case TK_ELLIPSIS:
    case TK_IF:
    case TK_FN:
    case TK_LET:
    case TK_QUOTE:
    case TK_RECUR:
    case TK_SET:
    case TK_RAISE:
    case TK_TRY:
      raise(snap, "Unexpect token on line %d", lex->line);
      break;
    case TK_EOF:
      break;
    default:
      raise(snap, "Invalid token on line %d", lex->line);
      break;
  }
  return create_nil();
}

static bool parse(Snap* snap, SnapLex* lex, SValue* result) {
  int token = snap_lex_next_token(lex);
  if (TK_EOF == token) return false;
  if (setjmp(snap->jmp) > 0) {
    // TODO: clear anchors
    *result = create_obj(snap->cause);
    return false;
  } else {
    *result = parse_expr(snap, lex, token);
    return true;
  }
}

static void load_constant(Snap* snap, SCodeGen* code_gen, SValue constant);
static void load_variable(Snap* snap, SCodeGen* code_gen, SSymStr* sym);

static void compile(Snap* snap, SCodeGen* code_gen, SValue expr) {
  int token;
  bool first = true;
  switch (expr.type) {
    case STYPE_INT:
    case STYPE_FLOAT:
    case STYPE_STR:
    case STYPE_BOOL:
    case STYPE_KEY:
      load_constant(snap, code_gen, expr);
      break;
    case STYPE_NIL:
      insts_append(snap, code_gen, LOAD_NIL);
      break;
    case STYPE_SYM:
      load_variable(snap, code_gen, as_sym(expr));
      break;
    case STYPE_CONS:
      compile_sexpr(snap, code_gen, as_cons(expr));
      break;
    case STYPE_FORM:
      // error
      break;
    default:
      // error
      raise(snap, "Unexpected type");
      break;
  }
  /*
  while ((token = snap_lex_next_token(lex)) != TK_EOF) {
    if (!first) insts_append(snap, code_gen, POP);
    compile_expr(snap, lex, code_gen, token);
    first = false;
  }
  */
  //snap_release(snap);
  //return snap_code_new(snap, code_gen);
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
  SValue key = create_obj(sym);
  int index = 0;
  if (snap_hash_get(&code_gen->scope->local_names, key)) {
    raise(snap, "Variable '%s' already defined in current scope", sym->data);
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
  SValue key = create_obj(sym);
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
  SValue key = create_obj(sym);
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

static void store_variable(Snap* snap, SCodeGen* code_gen, SSymStr* sym) {
  SScope* scope = code_gen->scope;
  SValue key = create_obj(sym);
  while (scope) {
    SValue* val = snap_hash_get(&scope->local_names, key);
    if (val) {
      insts_append(snap, code_gen, STORE_LOCAL)->arg = val->i;
      return;
    }
    scope = scope->up;
  }
  {
    int index = get_or_add_index(&code_gen->global_names, key);
    insts_append(snap, code_gen, STORE_GLOBAL)->arg = index;
  }
}

static void compile_def(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  SSymStr* sym = as_sym(sexpr->first);
  compile(snap, code_gen, as_cons(sexpr->rest)->first);
  if (code_gen->scope) {
    insts_append(snap, code_gen, STORE_LOCAL)->arg =
        add_local(snap, code_gen, sym);
  } else {
    insts_append(snap, code_gen, STORE_GLOBAL)->arg =
        add_global(code_gen, sym);
  }
  insts_append(snap, code_gen, LOAD_NIL); /* TODO: Hack */

#if 0
  SSymStr* sym;
  int token = snap_lex_next_token(lex);
  if (token != TK_ID) {
    raise(snap, "Expected id for first argument of define at %d", lex->line);
  }
  sym = anchor_sym(snap_sym_new(snap, lex->val));
  compile_expr(snap, lex, code_gen, snap_lex_next_token(lex));
  if (code_gen->scope) {
    insts_append(snap, code_gen, STORE_LOCAL)->arg = add_local(snap, code_gen, sym);
  } else {
    insts_append(snap, code_gen, STORE_GLOBAL)->arg = add_global(code_gen, sym);
  }
  insts_append(snap, code_gen, LOAD_NIL); /* TODO: Hack */
  snap_release(snap);
  token = snap_lex_next_token(lex);
  if (token != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }
#endif
}

static void compile_arith_expr(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  int i;
  int opcode = (sexpr->first.i - TK_ADD) + ADD;
  int count = compile_expr_list(snap, code_gen, as_cons(sexpr->rest));
  for (i = 1; i < count; ++i) {
    insts_append(snap, code_gen, opcode);
  }
}

static void compile_cond_expr_inner(Snap* snap, SCodeGen* code_gen, int opcode, SCons* sexpr) {
  SInst* jump_false = NULL;
  SCons* curr = as_cons(sexpr->rest);
  compile(snap, code_gen, sexpr->first);

  insts_append(snap, code_gen, DUP);
  insts_append(snap, code_gen, ROT3);
  insts_append(snap, code_gen, opcode);

  if (curr) {
    jump_false = insts_append(snap, code_gen, JUMP_FALSE_OR_POP);
    compile_cond_expr_inner(snap, code_gen, opcode, curr);
  }

  if (jump_false) {
    jump_arg_init(&jump_false->jump_arg, 1, code_gen->insts.prev);
  }
}

static void compile_cond_expr(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  int i;
  int opcode = (sexpr->first.i - TK_LT) + LESS_THAN;
  SCons* curr = as_cons(sexpr->rest);
  SInst* jump_false = NULL;

  for (i = 0; i < 2; ++i) {
    compile(snap, code_gen, curr->first);
    curr = as_cons(curr->rest);
  }

  if (curr) {
    insts_append(snap, code_gen, DUP);
    insts_append(snap, code_gen, ROT3);
    insts_append(snap, code_gen, opcode);
    jump_false = insts_append(snap, code_gen, JUMP_FALSE_OR_POP);
    compile_cond_expr_inner(snap, code_gen, opcode, curr);
  } else {
    insts_append(snap, code_gen, opcode);
  }

  if (jump_false) {
    jump_arg_init(&jump_false->jump_arg, 1, code_gen->insts.prev);
    insts_append(snap, code_gen, ROT2);
    insts_append(snap, code_gen, POP);
  }
}

static void compile_call(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  int count = compile_expr_list(snap, code_gen, as_cons(sexpr->rest));
  load_variable(snap, code_gen, as_sym(sexpr->first));
  insts_append(snap, code_gen, CALL)->arg = count;

#if 0
  int token;
  int count = 0;
  SSymStr* sym = anchor_sym(snap_sym_new(snap, lex->val));
  while ((token = snap_lex_next_token(lex)) != ')') {
    if (token == TK_EOF) break;
    compile_expr(snap, lex, code_gen, token);
    count++;
  }
  if (token != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }
  load_variable(snap, code_gen, sym);
  insts_append(snap, code_gen, CALL)->arg = count;
  snap_release(snap);
#endif
}

static void compile_if(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  SValue jump_val;
  SInst* cond_jump, * jump;

  compile(snap, code_gen, sexpr->first);
  cond_jump = insts_append(snap, code_gen, JUMP_FALSE);

  /* True */
  compile(snap, code_gen, as_cons(sexpr->rest)->first);
  jump = insts_append(snap, code_gen, JUMP);
  jump_arg_init(&cond_jump->jump_arg, 1, code_gen->insts.prev);

  /* False */
  compile(snap, code_gen, as_cons(as_cons(sexpr->rest)->rest)->first);
  jump_arg_init(&jump->jump_arg, 1, code_gen->insts.prev);

#if 0
  int token;
  SValue jump_val;
  SInst* cond_jump, * jump;
  compile_expr(snap, lex, code_gen, snap_lex_next_token(lex));
  cond_jump = insts_append(snap, code_gen, JUMP_FALSE);
  // true
  compile_expr(snap, lex, code_gen, snap_lex_next_token(lex));
  jump = insts_append(snap, code_gen, JUMP);
  jump_arg_init(&cond_jump->jump_arg, 1, code_gen->insts.prev);
  // false
  compile_expr(snap, lex, code_gen, snap_lex_next_token(lex));
  jump_arg_init(&jump->jump_arg, 1, code_gen->insts.prev);
  token = snap_lex_next_token(lex);
  if (token != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }
#endif
}

static void compile_set(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  compile(snap, code_gen, as_cons(sexpr->rest)->first);
  store_variable(snap, code_gen, as_sym(sexpr->first));
  insts_append(snap, code_gen, LOAD_NIL); /* TODO: Hack */
#if 0
  SSymStr* sym;
  int token = snap_lex_next_token(lex);
  if (token != TK_ID) {
    raise(snap, "Expected id for first argument of set! at %d", lex->line);
  }
  sym = anchor_sym(snap_sym_new(snap, lex->val));
  compile_expr(snap, lex, code_gen, snap_lex_next_token(lex));
  store_variable(snap, code_gen, sym);
  insts_append(snap, code_gen, LOAD_NIL); /* TODO: Hack */
  snap_release(snap);
  token = snap_lex_next_token(lex);
  if (token != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }
#endif
}

static void compile_fn(Snap* snap, SCodeGen* up, SCons* sexpr) {
  int i;
  SArr* args = as_arr(sexpr->first);
  SCodeGen* code_gen = snap_code_gen_new(snap, up);
  code_gen->scope = snap_scope_new(snap, NULL);

  for (i = 0; i < args->len; ++i) {
    SSymStr* sym = as_sym(args->data[i]);
    snap_vec_push(&code_gen->param_names, create_obj(sym));
    add_local(snap, code_gen, sym);
  }

  compile_expr_list(snap, code_gen, as_cons(sexpr->rest));

  insts_append(snap, code_gen, RETURN);
  load_constant(snap, up, create_obj(snap_code_new(snap, code_gen)));
#if 0
  // TODO: Anchor
  int token;
  SCodeGen* code_gen = snap_code_gen_new(snap, up);
  code_gen->scope = snap_scope_new(snap, NULL);
  token = snap_lex_next_token(lex);
  if (token != '[') {
    raise(snap, "Expected parameter list at %d", lex->line);
  }
  token = snap_lex_next_token(lex);
  while (token == TK_ID) {
    SSymStr* sym = snap_sym_new(snap, lex->val);
    snap_vec_push(&code_gen->param_names, create_obj(sym));
    add_local(snap, code_gen, sym);
    token = snap_lex_next_token(lex);
  }
  if (token != ']') {
    raise(snap, "Expected ']' to terminate parameter list at %d", lex->line);
  }
  token = compile_expr_list(snap, lex, code_gen);
  if (token != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }
  insts_append(snap, code_gen, RETURN);
  load_constant(snap, up, create_obj(snap_code_new(snap, code_gen)));
#endif
}

static void compile_let(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  // New scope
}

static void compile_recur(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  int i;
  SnapVec* param_names = &code_gen->param_names;
  int count = compile_expr_list(snap, code_gen, sexpr);
  if (count != param_names->size) {
    raise(snap, "Invalid number of arguments");
  }
  for (i = 0; i < count; ++i) {
    int index = get_or_add_index(&code_gen->scope->local_names, param_names->items[i]);
    insts_append(snap, code_gen, STORE_LOCAL)->arg =  index;
  }
  jump_arg_init(&insts_append(snap, code_gen, JUMP)->jump_arg, -1, code_gen->insts.next);
#if 0
  int i;
  int token;
  int count = 0;
  SnapVec* param_names = &code_gen->param_names;
  while ((token = snap_lex_next_token(lex)) != ')') {
    if (token == TK_EOF) break;
    compile_expr(snap, lex, code_gen, token);
    count++;
  }
  if (count != param_names->size) {
    raise(snap, "Invalid number of arguments", lex->line);
  }
  for (i = 0; i < count; ++i) {
    int index = get_or_add_index(&code_gen->scope->local_names, param_names->items[i]);
    insts_append(snap, code_gen, STORE_LOCAL)->arg =  index;
  }
  jump_arg_init(&insts_append(snap, code_gen, JUMP)->jump_arg, -1, code_gen->insts.next);
#endif
}

static void compile_catch(Snap* snap, SCodeGen* code_gen,
                          SCons* sexpr, SCons* next_catch) {
  bool empty_catch = true;
  SInst* jump_false = NULL;
  SnapNode* jump_to = NULL;
  SArr* arr;

  if (is_key(sexpr->first)) {
    insts_append(snap, code_gen, DUP);
    load_constant(snap, code_gen, sexpr->first);
    insts_append(snap, code_gen, EQUALS);
    jump_false = insts_append(snap, code_gen, JUMP_FALSE);
    empty_catch = false;
    sexpr = as_cons(sexpr->rest);
  }

  arr = as_arr(sexpr->first);

  code_gen->scope = snap_scope_new(snap, code_gen->scope);
  if (arr->len == 2) {
    store_variable(snap, code_gen, as_sym(arr->data[0]));
    store_variable(snap, code_gen, as_sym(arr->data[1]));
  } else if (arr->len == 1) {
    insts_append(snap, code_gen, POP);
    store_variable(snap, code_gen, as_sym(arr->data[0]));
  } else {
    insts_append(snap, code_gen, POP);
    insts_append(snap, code_gen, POP);
  }

  compile_expr_list(snap, code_gen, as_cons(sexpr->rest));
  code_gen->scope = code_gen->scope->up;

  if (next_catch) {
    SInst* jump = insts_append(snap, code_gen, JUMP);
    if (jump_false) {
      jump_arg_init(&jump_false->jump_arg, 1, code_gen->insts.prev);
      jump_false = NULL;
    }
    compile_catch(snap, code_gen,
                  as_cons(as_cons(next_catch->first)->rest),
                  as_cons(next_catch->rest));
    jump_arg_init(&jump->jump_arg, 1, code_gen->insts.prev);
  }

  if (!empty_catch && !next_catch) {
    SInst* jump = insts_append(snap, code_gen, JUMP);
    jump_to = insts_append(snap, code_gen, RAISE)->list.prev;
    jump_arg_init(&jump->jump_arg, 1, code_gen->insts.prev);
  } else {
    jump_to = code_gen->insts.prev;
  }

  if (jump_false) {
    jump_arg_init(&jump_false->jump_arg, 1, jump_to);
  }

#if 0
  int token;
  SInst* jump_false = NULL;
  SnapNode* jump_to = NULL;
  bool final_catch = false;
  int count = 0;
  const char* p;
  SSymStr* sym;


  if (empty_catch) {
    raise(snap, "empty catch is expect to be last at %d", lex->line);
  }

  if ((token = snap_lex_next_token(lex)) == TK_KEY) {
    insts_append(snap, code_gen, DUP);
    load_constant(snap, code_gen, create_obj(snap_key_new(snap, lex->val)));
    insts_append(snap, code_gen, EQUALS);
    jump_false = insts_append(snap, code_gen, JUMP_FALSE);
    token = snap_lex_next_token(lex);
  } else {
    empty_catch = true;
  }

  p = lex->p;
  if (token != '[') {
    raise(snap, "Expected '[' to for catch arguments at %d", lex->line);
  }

  while ((token = snap_lex_next_token(lex)) != ']') {
    if (token != TK_ID) {
      raise(snap, "Expected ids for catch arguments at %d", lex->line);
    }
    count++;
  }
  if (token != ']') {
    raise(snap, "Expected ']' to terminate catch arguments at %d", lex->line);
  }
  lex->p = p;

  if (count > 2) {
    raise(snap, "Invalid number of catch argument (0 - 2) %d", lex->line);
  }

  code_gen->scope = snap_scope_new(snap, code_gen->scope);
  if (count == 2) {
    snap_lex_next_token(lex);
    sym = anchor_sym(snap_sym_new(snap, lex->val));
    store_variable(snap, code_gen, sym);
    snap_release(snap);
    snap_lex_next_token(lex);
    sym = anchor_sym(snap_sym_new(snap, lex->val));
    store_variable(snap, code_gen, sym);
    snap_release(snap);
  } else if (count == 1) {
    insts_append(snap, code_gen, POP);
    snap_lex_next_token(lex);
    sym = anchor_sym(snap_sym_new(snap, lex->val));
    store_variable(snap, code_gen, sym);
    snap_release(snap);
  } else {
    insts_append(snap, code_gen, POP);
    insts_append(snap, code_gen, POP);
  }
  snap_lex_next_token(lex); /* Consume ']' */

  token = compile_expr_list(snap, lex, code_gen);
  code_gen->scope = code_gen->scope->up;

  if (token != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }

  if ((token = snap_lex_next_token(lex)) == '(')  {
    if ((token = snap_lex_next_token(lex)) == TK_CATCH) {
      SInst* jump = insts_append(snap, code_gen, JUMP);
      if (jump_false) {
        jump_arg_init(&jump_false->jump_arg, 1, code_gen->insts.prev);
        jump_false = NULL;
      }
      token = compile_catch(snap, lex, code_gen, empty_catch);
      jump_arg_init(&jump->jump_arg, 1, code_gen->insts.prev);
    } else {
      final_catch = true;
    }
  } else {
      final_catch = true;
  }

  if (!empty_catch && final_catch) {
    SInst* jump = insts_append(snap, code_gen, JUMP);
    jump_to = insts_append(snap, code_gen, RAISE)->list.prev;
    jump_arg_init(&jump->jump_arg, 1, code_gen->insts.prev);
  } else {
    jump_to = code_gen->insts.prev;
  }

  if (jump_false) {
    jump_arg_init(&jump_false->jump_arg, 1, jump_to);
  }

  return token;
#endif
}

static void compile_try(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  /* (try <exprs> ...
   *  (catch :err1 [<args> ...] <exprs> ...)
   *  (catch :err2 [<args> ...] <exprs> ...)
   *  ...
   *  (catch [<args> ...] <exprs> ...)
   * )
   */
  SCons* curr;
  SInst* jump_catch, * jump;
  bool first = true;

  jump_catch = insts_append(snap, code_gen, BLOCK);

  for (curr = sexpr; curr != NULL; curr = as_cons(curr->rest)) {
    if (is_cons(curr->first) &&
        is_form(as_cons(curr->first)->first) &&
        as_cons(curr->first)->first.i == TK_CATCH) {
      break;
    }
    if (!first) insts_append(snap, code_gen, POP);
    first = false;
    compile(snap, code_gen, curr->first);
  }

  insts_append(snap, code_gen, END_BLOCK);
  jump = insts_append(snap, code_gen, JUMP);

  jump_arg_init(&jump_catch->jump_arg, 1, code_gen->insts.prev);

  compile_catch(snap, code_gen,
                as_cons(as_cons(curr->first)->rest),
                as_cons(curr->rest));

  jump_arg_init(&jump->jump_arg, 1, code_gen->insts.prev);

#if 0
  int token;
  int count = 0;
  int index;
  bool empty_catch = false;
  SInst* jump_catch, * jump;

  jump_catch = insts_append(snap, code_gen, BLOCK);

  while ((token = snap_lex_next_token(lex)) != ')') {
    if (token == TK_EOF) break;
    if (token == '(') {
      token = snap_lex_next_token(lex);
      if (token == TK_CATCH) {
        break;
      } else {
        if (!first) insts_append(snap, code_gen, POP);
        first = false;
        compile_sexpr(snap, lex, code_gen, token);
        count++;
      }
    } else {
      if (!first) insts_append(snap, code_gen, POP);
      first = false;
      compile_expr(snap, lex, code_gen, token);
      count++;
    }
  }

  if (count < 1) {
    compile_err(snap, "Expression expected at %d", lex->line);
  }

  if (token != TK_CATCH) {
    compile_err(snap, "catch expected at %d", lex->line);
  }

  insts_append(snap, code_gen, END_BLOCK);
  jump = insts_append(snap, code_gen, JUMP);

  jump_arg_init(&jump_catch->jump_arg, 1, code_gen->insts.prev);

  token = compile_catch(snap, lex, code_gen, false);

  jump_arg_init(&jump->jump_arg, 1, code_gen->insts.prev);

  if (token != ')') {
    compile_err(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }
#endif
}

static void compile_raise(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  compile(snap, code_gen, as_cons(sexpr->rest)->first);
  load_constant(snap, code_gen, sexpr->first);
  insts_append(snap, code_gen, RAISE);
#if 0
  SInst* raise;
  int token = snap_lex_next_token(lex);
  int count = 1;

  SKeyword* key;

  if (token == TK_KEY) {
    key = anchor_key(snap_key_new(snap, lex->val));
  } else {
    key = anchor_key(snap_key_new(snap, "error"));
  }

  if ((token = snap_lex_next_token(lex)) != ')') {
    compile_expr(snap, lex, code_gen, token);
  } else {
    insts_append(snap, code_gen, LOAD_NIL);
  }

  if ((token = snap_lex_next_token(lex)) != ')') {
    raise(snap, "Expected ')' to terminate s-expression at %d", lex->line);
  }

  load_constant(snap, code_gen, create_obj(key));
  insts_append(snap, code_gen, RAISE);

  snap_release(snap);
#endif
}

static void compile_sexpr(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  switch (sexpr->first.type) {
    case STYPE_SYM:
      compile_call(snap, code_gen, sexpr);
      break;
    case STYPE_FORM:
      switch (sexpr->first.i) {
        case TK_DO:
          compile_expr_list(snap, code_gen, as_cons(sexpr->rest));
          break;
        case TK_DEF:
          compile_def(snap, code_gen, as_cons(sexpr->rest));
          break;
        case TK_IF:
          compile_if(snap, code_gen, as_cons(sexpr->rest));
          break;
        case TK_FN:
          compile_fn(snap, code_gen, as_cons(sexpr->rest));
          break;
        case TK_LET:
          compile_let(snap, code_gen, as_cons(sexpr->rest));
          break;
        case TK_QUOTE:
          load_constant(snap, code_gen, as_cons(sexpr->rest)->first);
          break;
        case TK_RECUR:
          compile_recur(snap, code_gen, as_cons(sexpr->rest));
          break;
        case TK_SET:
          compile_set(snap, code_gen, as_cons(sexpr->rest));
          break;
        case TK_RAISE:
          compile_raise(snap, code_gen, as_cons(sexpr->rest));
          break;
        case TK_TRY:
          compile_try(snap, code_gen, as_cons(sexpr->rest));
          break;
        case TK_ADD: case TK_SUB: case TK_MUL: case TK_DIV: case TK_MOD:
          compile_arith_expr(snap, code_gen, sexpr);
          break;
        case TK_LT: case TK_LE: case TK_GT: case TK_GE: case TK_EQ:
          compile_cond_expr(snap, code_gen, sexpr);
          break;
      }
      break;
    default:
      raise(snap, "Expected id, special form or expr");
      break;
  }
#if 0
  switch (token) {
    case ')':
      break;
    case TK_ID:
      compile_call(snap, lex, code_gen);
      break;
    default:
      compile_err(snap, "Expected id, special form or expr at %d", lex->line);
      break;
  }
#endif
}

static void compile_expr(Snap* snap, SnapLex* lex, SCodeGen* code_gen, int token) {
#if 0
  switch (token) {
    case '(':
      compile_sexpr(snap, lex, code_gen, snap_lex_next_token(lex));
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
      load_constant(snap, code_gen, create_obj(snap_str_new(snap, lex->val)));
      break;
    case TK_ID:
      load_variable(snap, code_gen, anchor_sym(snap_sym_new(snap, lex->val)));
      snap_release(snap);
      break;
    case TK_KEY:
      load_constant(snap, code_gen, create_obj(snap_key_new(snap, lex->val)));
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
    case TK_RAISE:
    case TK_TRY:
      // error
      break;
    case TK_EOF:
      break;
    default:
      compile_err(snap, "Invalid token on line %d", lex->line);
      break;
  }
#endif
}

static int compile_expr_list(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  SCons* curr;
  int count = 0;
  for (curr = sexpr; curr != NULL; curr = as_cons(curr->rest)) {
    if (count > 0) insts_append(snap, code_gen, POP);
    compile(snap, code_gen, curr->first);
    count++;
  }
  return count;
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
        case TK_RAISE: iprintf(indent, "raise"); break;
        case TK_TRY: iprintf(indent, "try"); break;
        case TK_CATCH: iprintf(indent, "catch"); break;
        case TK_LT: iprintf(indent, "<"); break;
        case TK_ADD: iprintf(indent, "+"); break;
      }
      break;
    case STYPE_STR:
      iprintf(indent, "\"%s\"", as_str(val)->data);
      break;
    case STYPE_SYM:
      iprintf(indent, "%s", as_sym(val)->data);
      break;
    case STYPE_KEY:
      iprintf(indent, ":%s", as_key(val)->data);
      break;
    case STYPE_ERR:
      iprintf(indent, "<err %s ", as_err(val)->err->data);
      print_val(as_err(val)->msg, 0);
      printf(">");
      break;
    case STYPE_ARR:
      iprintf(indent, "[");
      print_arr(as_arr(val), indent);
      iprintf(indent, "]");
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

static void print_arr(SArr* arr, int indent) {
  int i = 0;
  for (i = 0; i < arr->len; ++i) {
    if (i > 0) printf(" ");
    print_val(arr->data[i], indent);
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
                      op == JUMP || op == JUMP_FALSE || op == JUMP_FALSE_OR_POP || op == BLOCK \
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
      case JUMP_FALSE_OR_POP:
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
                      inst->arg == JUMP || inst->arg == JUMP_FALSE || inst->arg == JUMP_FALSE_OR_POP \
                      ? inst->arg - MAX_OP_ARG : inst->arg); } \
        break;
      OPCODE_MAP(XX)
    #undef XX
    }
    i++;
  }
}

void builtin_print(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args > 0) {
    int i;
    for (i = 0; i < num_args; ++i) {
      if (i > 0) printf(" ");
      snap_print(args[i]);
    }
  }
}

void builtin_println(Snap* snap, const SValue* args, int num_args, SValue* result) {
  builtin_print(snap, args, num_args, result);
  printf("\n");
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
  snap_hash_init(&snap->keywords);

  snap_define(snap, "print", create_cfunc(builtin_print));
  snap_define(snap, "println", create_cfunc(builtin_println));
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
  snap_hash_destroy(&snap->keywords);
  assert(snap->num_bytes_alloced == 0);
}
