#include "snap.h"
#include "snap_lex.h"

#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define GC_EVERY_NUM_BYTES 64 * 1024 * 1024

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
  XX(LOAD_CLOSED, "LOAD_CLOSED", true) \
  XX(STORE_CLOSED, "STORE_CLOSED", true) \
  XX(LOAD_CONSTANT, "LOAD_CONSTANT", true) \
  XX(LOAD_NIL, "LOAD_NIL", false) \
  XX(POP, "POP", false) \
  XX(DUP, "DUP", false) \
  XX(ROT2, "ROT2", false) \
  XX(ROT3, "ROT3", false) \
  XX(CALL, "CALL", true) \
  XX(CLOSURE, "CLOSURE", false) \
  XX(RETURN, "RETURN", false) \
  XX(JUMP, "JUMP", true) \
  XX(JUMP_FALSE, "JUMP_FALSE", true) \
  XX(JUMP_FALSE_OR_POP, "JUMP_FALSE_OR_POP", true) \
  XX(JUMP_TRUE, "JUMP_TRUE", true) \
  XX(JUMP_TRUE_OR_POP, "JUMP_TRUE_OR_POP", true) \
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
  XX(EQUALS, "EQUALS", false) \
  XX(NOT, "NOT", false)

enum {
#define XX(opcode, name, has_arg) opcode,
  OPCODE_MAP(XX)
#undef XX
};

static const char* opcode_name(int op) {
  switch (op) {
#define XX(opcode, name, has_arg) \
    case opcode: \
      return name;
    OPCODE_MAP(XX)
#undef XX
    default:
      return "INVALID";
  }
}

static const char* stype_name(int  type) {
  switch (type) {
#define XX(type, id, name) \
    case type: \
      return name;
    STYPE_MAPPING(XX)
#undef XX
    default:
      return "invalid";
  }
}

#define compare(a, b) ((a) < (b) ? -1 : (a) > (b))

#define isclosed(c) ((c).value == &(c).closed)

#define relocate(stack_old, stack_new, stack_curr) \
  stack_curr = ((stack_new) + ((stack_curr) - (stack_old)))

static int hash_float(double n) {
  int i;
  unsigned int u;
  if (!isfinite(n)) return 0;
  n = frexp(n, &i) * -(double)INT_MIN;
  u = (unsigned int)n + (unsigned int)i;
  return (int)(u <= (unsigned int)INT_MAX ? u : ~u);
}

static int hash_string(SSymStr* s) {
  return (int)mhash_fnv1a((const void*)s->data, s->len);
}

static int hash_keyword(SKeyword* k) {
  return (int)mhash_fnv1a((const void*)k->data, k->len);
}

static int hash_tempstr(STempStr* ts) {
  return (int)mhash_fnv1a((const void*)ts->data, ts->len);
}

static int compare_string(SSymStr* s1, SSymStr* s2) {
  if (s1->len != s2->len) {
    return s1->len < s2->len ? -1 : 1;
  }
  return strncmp(s1->data, s2->data, s1->len);
}

static int compare_tempstring(const STempStr* ts, const SValue* val) {
  SSymStr* s;
  SKeyword* k;
  switch (val->type) {
  case STYPE_SYM:
  case STYPE_STR:
    s = (SSymStr*)val->o;
    if (ts->len < s->len) {
      return ts->len < s->len ? -1 : 1;
    }
    return strncmp(ts->data, s->data, ts->len);
  case STYPE_KEY:
    k = (SKeyword*)val->o;
    if (ts->len < k->len) {
      return ts->len < k->len ? -1 : 1;
    }
    return strncmp(ts->data, k->data, ts->len);
  default:
    return STYPE_TEMPSTR < val->type ? -1 : 1;
  }
}

static int hash_pointer(void* ptr) {
  uintptr_t p = (uintptr_t)ptr;
  return (int)((p >> 4) | (p << (8 * sizeof(ptr) - 4)));
}

int snap_hash(const SValue* val) {
  switch (val->type) {
    case STYPE_NIL:
      return 0;
    case STYPE_BOOL:
      return val->b ? 1 : 0;
    case STYPE_INT:
      return val->i;
    case STYPE_FLOAT:
      return hash_float(val->f);
    case STYPE_FORM:
      return val->i;
    case STYPE_CFUNC:
      return hash_pointer((void*)val->c);
    case STYPE_SYM:
    case STYPE_STR:
      return hash_string((SSymStr*)val->o);
    case STYPE_KEY:
      return hash_keyword((SKeyword*)val->o);
    case STYPE_TEMPSTR:
      return hash_tempstr((STempStr*)val->o);
    case STYPE_ERR:
    case STYPE_CONS:
    case STYPE_SCOPE:
    case STYPE_CODE:
    case STYPE_CODE_GEN:
      return hash_pointer((void*)val->o);
  }
  return 0;
}

int snap_compare(const SValue* val1, const SValue* val2) {
  assert(val2->type != STYPE_TEMPSTR);

  if (val1->type == STYPE_TEMPSTR) {
    return compare_tempstring((STempStr*)val1->o, val2);
  } else if (val1->type != val2->type) {
    return val1->type < val2->type ? -1 : 1;
  }

  switch (val1->type) {
    case STYPE_NIL:
      return 0;
    case STYPE_BOOL:
      return compare(val1->b, val2->b);
    case STYPE_INT:
      return compare(val1->i, val2->i);
    case STYPE_FLOAT:
      return compare(val1->f, val2->f);
    case STYPE_FORM:
      return compare(val1->i, val2->i);
    case STYPE_CFUNC:
      return compare((void*)val1->c, (void*)val2->c);
    case STYPE_SYM:
    case STYPE_STR:
      return compare_string((SSymStr*)val1->o, (SSymStr*)val2->o);
    case STYPE_KEY:
      return compare(((SKeyword*)val1->o)->id, ((SKeyword*)val2->o)->id);
    case STYPE_ERR:
      return snap_compare(&((SErr*)val1->o)->err, &((SErr*)val2->o)->err);
    case STYPE_CONS:
    case STYPE_SCOPE:
    case STYPE_CODE:
    case STYPE_CODE_GEN:
      return compare(val1->o, val2->o);
  }
  return 0;
}

#define anchor(type, obj) (type*)snap_anchor(snap, (SObject*)obj)
#define anchor_arr(arr) anchor(SArr, arr)
#define anchor_sym(sym) anchor(SSymStr, sym)
#define anchor_str(str) anchor(SSymStr, str)
#define anchor_key(key) anchor(SKeyword, key)
#define anchor_cons(cons) anchor(SCons, cons)
#define anchor_code(code) anchor(SCode, code)
#define anchor_code_gen(code_gen) anchor(SCodeGen, code_gen)

static SValue exec(Snap* snap);
static void compile(Snap* snap, SCodeGen* code_gen, SValue value);
static bool parse(Snap* snap, SnapLex* lex, SValue *result);

static SInst* insts_append(Snap* snap, SCodeGen* code_gen, int opcode);
static int insts_calculate_jump(SInst* inst);
static void insts_remove_dead_code(MList* insts);
static void insts_dump(MList* insts, int indent);

static void print_val(SValue val, int indent);
static void print_code(SCode* code, int indent);
static void print_closure(SClosure* closure, int indent);
static void print_cons(SCons* cons, int indent);
static void print_arr(SArr* arr, int indent);

static void jump_arg_init(SnapJumpArg* jump_arg, int dir, MList* dest) {
  jump_arg->dir = dir;
  jump_arg->dest = dest;
}

SValue create_undef() {
  SValue val;
  val.type = STYPE_UNDEF;
  val.o = NULL;
  return val;
}

SValue create_deleted() {
  SValue val;
  val.type = STYPE_DELETED;
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

static inline MHASH_FUNC(SnapHashEntry, SValue) {
  return snap_hash(&key);
}

static inline MHASH_EQUALS_FUNC(SnapHashEntry, SValue) {
  return snap_compare(&key1, &key2) == 0;
}

void snap_hash_init(SnapHash* hash) {
  mhash_init(hash, SnapHashEntry, create_undef(), create_deleted(), 0);
}

void snap_hash_put(SnapHash* hash, SValue key, SValue val) {
  SnapHashEntry* entry;
  mhash_find(hash, SnapHashEntry, key, entry, 1);
  entry->val = val;
  if (mhash_is_empty(hash, SnapHashEntry, entry) ||
      mhash_is_deleted(hash, SnapHashEntry, entry)) {
    entry->hkey = key;
    mhash_add(hash, SnapHashEntry);
  }
}

SValue* snap_hash_get(SnapHash *hash, SValue key) {
  SnapHashEntry* entry;
  mhash_find(hash, SnapHashEntry, key, entry, 0);
  if (mhash_is_empty(hash, SnapHashEntry, entry)) {
    return NULL;
  }
  return &entry->val;
}

SValue* snap_hash_get_str(SnapHash* hash, const char* key, size_t len) {
  STempStr ts;
  ts.type = STYPE_TEMPSTR;
  ts.data = key;
  ts.len = len;
  return snap_hash_get(hash, create_obj(&ts));
}

void snap_hash_destroy(SnapHash* hash) {
  mhash_destroy(hash);
}

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
      mvec_destroy(&((SScope*)obj)->param_names);
      break;
    case STYPE_CODE_GEN:
      snap->num_bytes_alloced -= sizeof(SCodeGen);
      snap_hash_destroy(&((SCodeGen*)obj)->global_names);
      snap_hash_destroy(&((SCodeGen*)obj)->closed_names);
      mvec_destroy(&((SCodeGen*)obj)->closed_descs);
      snap_hash_destroy(&((SCodeGen*)obj)->constants);
      mvec_destroy(&((SCodeGen*)obj)->param_names);
      break;
    case STYPE_CODE:
      snap->num_bytes_alloced -= sizeof(SCode);
      free(((SCode*)obj)->insts);
      break;
    case STYPE_CLOSURE:
      snap->num_bytes_alloced -= (sizeof(SClosure) + ((SClosure*)obj)->code->closed_descs->len * sizeof(SnapClosed));
      break;
    case STYPE_CLOSED_DESC:
      snap->num_bytes_alloced -= sizeof(SClosedDesc);
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
    case STYPE_INST:
    case STYPE_CODE_GEN:
    case STYPE_CODE:
    case STYPE_CLOSURE:
    case STYPE_CLOSED_DESC:
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
  for (i = 0; i < hash->hcapacity; ++i) {
    SnapHashEntry* entry = &hash->hentries[i];
    if (!is_undef(entry->hkey)) {
      gc_mark_val(snap, entry->hkey);
      gc_mark_val(snap, entry->val);
    }
  }
}

static void gc_mark_vec(Snap* snap, SnapVec* vec) {
  SValue* item;
  mvec_foreach(vec, item) {
    if (!is_undef_p(item)) {
      gc_mark_val(snap, *item);
    }
  }
}

static void gc_mark_children(Snap* snap, SObject* obj) {
  int i;
  switch (obj->type) {
    case STYPE_ERR:
      gc_mark_val(snap, ((SErr*)obj)->err);
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
      gc_mark_vec(snap, &((SScope*)obj)->param_names);
      break;
    case STYPE_INST:
      gc_mark_val(snap, ((SInst*)obj)->arg_data);
      gc_mark(snap, (SObject*)((SInst*)obj)->list.next);
      break;
    case STYPE_CODE_GEN:
      gc_mark(snap, (SObject*)((SCodeGen*)obj)->scope);
      gc_mark_hash(snap, &((SCodeGen*)obj)->constants);
      gc_mark_hash(snap, &((SCodeGen*)obj)->global_names);
      gc_mark_hash(snap, &((SCodeGen*)obj)->closed_names);
      gc_mark_vec(snap, &((SCodeGen*)obj)->closed_descs);
      gc_mark_vec(snap, &((SCodeGen*)obj)->param_names);
      break;
    case STYPE_CODE:
      gc_mark(snap, (SObject*)((SCode*)obj)->constants);
      gc_mark(snap, (SObject*)((SCode*)obj)->global_names);
      gc_mark(snap, (SObject*)((SCode*)obj)->closed_descs);
      gc_mark(snap, (SObject*)((SCode*)obj)->insts_debug.next);
    case STYPE_CLOSURE:
      gc_mark(snap, (SObject*)((SClosure*)obj)->code);
      for (i = 0; i < ((SClosure*)obj)->code->closed_descs->len; ++i) {
        if (isclosed(((SClosure*)obj)->closed[i])) {
          gc_mark_val(snap, ((SClosure*)obj)->closed[i].closed);
        }
      }
      break;
    case STYPE_CLOSED_DESC:
      gc_mark(snap, (SObject*)((SClosedDesc*)obj)->name);
      break;
    default:
      assert(0 && "Not a valid gray object");
      break;
  }
}

static void gc_collect(Snap* snap) {
  SObject** obj;

  mvec_foreach(&snap->anchors, obj) {
    gc_mark(snap, *obj);
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

SSymStr* snap_str_new_vformat(Snap* snap, const char* format, va_list args) {
  SSymStr* s;
  char buf[256]; /* TODO: Dynamic buffer size */
  int len = vsnprintf(buf, sizeof(buf), format, args);
  s = (SSymStr*)gc_new(snap, STYPE_STR, sizeof(SSymStr) + len + 1);
  s->len = len;
  strncpy(s->data, buf, len);
  return s;
}

SSymStr* snap_str_new_format(Snap* snap, const char* format, ...) {
  SSymStr* s;
  va_list args;
  va_start(args, format);
  s = snap_str_new_vformat(snap, format, args);
  va_end(args);
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
    k->id = snap->keywords.hcount;
    snap_hash_put(&snap->keywords, create_obj(k), create_obj(k));
    return k;
  }
}

SErr* snap_err_new(Snap* snap, SValue err, SValue msg) {
  SErr* e = (SErr*)gc_new(snap, STYPE_ERR, sizeof(SErr));
  e->err = err;
  e->msg = msg;
  return e;
}

SErr* snap_err_new_format(Snap* snap, const char* err, const char* format, ...) {
  SSymStr* s;
  SKeyword* k;
  SErr* e;
  va_list args;
  va_start(args, format);
  s = anchor_str(snap_str_new_vformat(snap, format, args));
  va_end(args);
  k = anchor_key(snap_key_new(snap, err));
  e = snap_err_new(snap, create_obj(k), create_obj(s));
  snap_release(snap);
  snap_release(snap);
  return e;
}

SCons* snap_cons_new(Snap* snap) {
 SCons* c = (SCons*)gc_new(snap,  STYPE_CONS, sizeof(SCons));
 c->first = create_undef();
 c->rest = create_empty();
 return c;
}

SArr* snap_arr_new(Snap* snap, int len) {
  SArr* a = (SArr*)gc_new(snap, STYPE_ARR, sizeof(SArr) + len * sizeof(SValue));
  a->len = len;
  return a;
}

SArr* snap_arr_hash_new(Snap* snap, SnapHash* hash) {
  int i;
  SArr* a = snap_arr_new(snap, hash->hcount);
  for (i = 0; i < hash->hcapacity; ++i) {
    SnapHashEntry* entry = &hash->hentries[i];
    if (!is_undef(entry->hkey)) {
      a->data[entry->val.i] = entry->hkey;
    }
  }
  return a;
}

SArr* snap_arr_vec_new(Snap* snap, SnapVec* vec) {
  int i;
  SArr* a = snap_arr_new(snap, vec->vsize);
  for (i = 0; i < vec->vsize; ++i) {
    a->data[i] = vec->vitems[i];
  }
  return a;
}

SClosedDesc* snap_closed_desc_new(Snap* snap, bool islocal,
                                  int index, SSymStr* name) {
  SClosedDesc* d = (SClosedDesc*)gc_new(snap, STYPE_CLOSED_DESC, sizeof(SClosedDesc));
  d->islocal = islocal;
  d->index = index;
  d->name = name;
  return d;
}

SClosure* snap_closure_new(Snap* snap, SCode* code, SClosure* next) {
  int num_closed = code->closed_descs->len;
  SClosure* cl = (SClosure*)gc_new(snap,
                                   STYPE_CLOSURE,
                                   sizeof(SClosure) +
                                   sizeof(SnapClosed) *
                                   num_closed);
  cl->code = code;
  cl->next = next;
  return cl;
}

SCode* snap_code_new(Snap* snap, SCodeGen* code_gen) {
  SCode* c = (SCode*)gc_new(snap, STYPE_CODE, sizeof(SCode));
  snap_anchor(snap, (SObject*)c);

  //insts_dump(&code_gen->insts, 0);
  //printf("\n");
  insts_remove_dead_code(&code_gen->insts);
  //insts_dump(&code_gen->insts, 0);
  //printf("\n");

  c->insts = (int*)malloc(sizeof(int) * code_gen->insts_count);
  {
    int count = 0;
    int stack_size = 0;
    int max_stack_size = 0;
    MList* pos;
    mlist_foreach(&code_gen->insts) {
      SInst* inst = mlist_entry(SInst, pos, list);
      switch(inst->opcode) {
        case LOAD_GLOBAL: case LOAD_LOCAL: case LOAD_CLOSED:
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
          stack_size--;
          break;
        case JUMP_FALSE:
          stack_size--;
          break;
        case BLOCK:
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
      if (inst->opcode == JUMP ||
          inst->opcode == JUMP_FALSE ||
          inst->opcode == JUMP_FALSE_OR_POP ||
          inst->opcode == JUMP_TRUE ||
          inst->opcode == JUMP_TRUE_OR_POP ||
          inst->opcode == BLOCK) {
        int jump = insts_calculate_jump(inst) + MAX_OP_ARG;
        c->insts[count++] = ((inst->opcode & 0x3F) << 26) | (jump & 0x3FFFFFF);
      } else {
        c->insts[count++] = ((inst->opcode & 0x3F) << 26) | (inst->arg & 0x3FFFFFF);
      }
    }
    c->num_locals = code_gen->num_locals;
    c->num_args = code_gen->param_names.vsize;
    c->max_stack_size = max_stack_size + 2; /* Add enough room for raising errors */
    c->insts_count = count;
  }
  c->constants = snap_arr_hash_new(snap, &code_gen->constants);
  c->global_names = snap_arr_hash_new(snap, &code_gen->global_names);
  c->closed_descs = snap_arr_vec_new(snap, &code_gen->closed_descs);
  mlist_copy(&code_gen->insts, &c->insts_debug);
  snap_release(snap);
  return c;
}

SInst* snap_inst_new(Snap* snap, int opcode) {
 SInst* i = (SInst*)gc_new(snap,  STYPE_INST, sizeof(SInst));
 i->opcode = opcode;
 i->arg = 0;
 i->arg_data = create_nil();
 jump_arg_init(&i->jump_arg, 0, NULL);
 i->list.next = i->list.prev = NULL;
 return i;
}

SScope* snap_scope_new(Snap* snap, SScope* up) {
  SScope* s = (SScope*)gc_new(snap,  STYPE_SCOPE, sizeof(SScope));
  s->up = up;
  snap_hash_init(&s->local_names);
  mvec_init(&s->param_names, SValue, 2);
  return s;
}

SCodeGen* snap_code_gen_new(Snap* snap, SCodeGen* up) {
  SCodeGen* c = (SCodeGen*)gc_new(snap,  STYPE_CODE_GEN, sizeof(SCodeGen));
  mlist_init(&c->insts);
  c->insts_count = 0;
  c->scope = NULL;
  snap_hash_init(&c->constants);
  snap_hash_init(&c->global_names);
  snap_hash_init(&c->closed_names);
  mvec_init(&c->closed_descs, SValue, 2);
  mvec_init(&c->param_names, SValue, 2);
  c->num_locals = 0;
  c->is_tail = false;
  c->up = up;
  return c;
}

void snap_def(Snap* snap, const char* name, SValue val) {
  SValue key = create_obj(snap_sym_new(snap, name));
  snap_hash_put(&snap->globals, key, val);
}

void snap_def_cfunc(Snap* snap, const char* name, SCFunc cfunc) {
  snap_def(snap, name, create_cfunc(cfunc));
}

// Stack layout
// [args][locals][stack...]

// During call
// 1) [args and locals][stack...][calling args][function]
// 2) [args and locals][stack...][args and locals][stack...]
// 3) [args and locals][stack...][return value]

static void push_frame(Snap* snap, SCode* code, SValue* stack_base, SClosure* closure) {
  SnapFrame* frame = mvec_push(&snap->frames, SnapFrame);

  frame->code = code;
  frame->closure = closure;
  frame->enclosed = NULL;
  frame->pc = code->insts;
  frame->blocks_top = 0;
  frame->stack_base = stack_base;
  frame->stack_top = stack_base + code->num_locals;

  if (stack_base + code->max_stack_size >= snap->stack_end) {
    int stack_size = snap->stack_size * (snap->stack_size < 4096 ? 4 : 2) + code->max_stack_size;
    SValue* stack_old = snap->stack;
    SValue* stack_new = (SValue*)realloc(snap->stack, stack_size * sizeof(SValue));
    snap->stack_size = stack_size;
    snap->stack = stack_new;
    snap->stack_end = stack_new + stack_size;

    // Update stack pointers to the new stack
    mvec_foreach(&snap->frames, frame) {
      SClosure* enclosed;

      relocate(stack_old, stack_new, frame->stack_base);
      relocate(stack_old, stack_new, frame->stack_top);

      enclosed = frame->enclosed;
      while (enclosed) {
        int i;
        for (i = 0; i < enclosed->code->closed_descs->len; ++i) {
          if (!isclosed(enclosed->closed[i])) {
            relocate(stack_old, stack_new, enclosed->closed[i].value);
          }
        }
        enclosed  = enclosed->next;
      }
    }
  }
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

#define GETOP() (((*pc) >> 26) & 0x3F)
#define GETARG() ((*pc) & 0x3FFFFFF)
#define GETJARG() (((*pc) & 0x3FFFFFF) - MAX_OP_ARG)

#define NEXT(n) pc += n

#define INDIRECT_DISPATCH 1

#ifdef INDIRECT_DISPATCH
#define OP(name) _##name:
#define OPT(name) [name] = &&_##name
#define DISPATCH() goto *optable[GETOP()]
#else
#define OP(name) case name:
#define DISPATCH() continue
#endif

static SValue exec(Snap* snap) {
  SnapFrame* frame;
  SnapBlock* block;
  SCode* code;
  SClosure* closure;
  int* pc;
  SValue* constants;
  SValue* global_names;
  SValue* locals;
  SValue* stack;
  SValue* a, * b, * r;
  SValue temp;
  int arg;

#ifdef INDIRECT_DISPATCH
#define XX(opcode, name, has_arg) OPT(opcode),
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
    constants = code->constants->data;
    global_names = code->global_names->data;
    locals = frame->stack_base;
    stack = frame->stack_top;

#ifdef INDIRECT_DISPATCH
    DISPATCH();
#endif

#define RUNTIME_ERROR(err,  ...)                           \
  *stack++ = create_obj(snap_str_new_format(snap, __VA_ARGS__)); \
  *stack++ = create_obj(snap_key_new(snap, err));          \
  goto raise;

#ifndef INDIRECT_DISPATCH
  for (;;) { switch (GETOP()) {
#endif
      OP(LOAD_GLOBAL) {
        arg = GETARG();
        NEXT(1);
        a = snap_hash_get(&snap->globals, global_names[arg]);
        if (!a) {
          RUNTIME_ERROR("snap_name_error", "'%s' is not defined", as_sym(global_names[arg])->data);
        }
        *stack++ = *a;
        DISPATCH();
      }
      OP(STORE_GLOBAL) {
        arg = GETARG();
        NEXT(1);
        snap_hash_put(&snap->globals, global_names[arg], *--stack);
        DISPATCH();
      }
      OP(LOAD_LOCAL) {
        arg = GETARG();
        NEXT(1);
        *stack++ = locals[arg];
        DISPATCH();
      }
      OP(STORE_LOCAL) {
        arg = GETARG();
        NEXT(1);
        locals[arg] = *--stack;
        DISPATCH();
      }
      OP(LOAD_CLOSED) {
        arg = GETARG();
        NEXT(1);
        *stack++ = *closure->closed[arg].value;
        DISPATCH();
      }
      OP(STORE_CLOSED) {
        arg = GETARG();
        NEXT(1);
        *closure->closed[arg].value = *--stack;
        DISPATCH();
      }
      OP(LOAD_CONSTANT) {
        arg = GETARG();
        NEXT(1);
        *stack++ = constants[arg];
        DISPATCH();
      }
      OP(LOAD_NIL) {
        arg = GETARG();
        NEXT(1);
        *stack++ = create_nil();
        DISPATCH();
      }
      OP(POP) {
        NEXT(1);
        --stack;
        DISPATCH();
      }
      OP(DUP) {
        NEXT(1);
        *stack = *(stack - 1);
        stack++;
        DISPATCH();
      }
      OP(ROT2) {
        NEXT(1);
        temp = *(stack - 1);
        *(stack - 1) = *(stack - 2);
        *(stack - 2) = temp;
        DISPATCH();
      }
      OP(ROT3) {
        NEXT(1);
        temp = *(stack - 3);
        *(stack - 3) = *(stack - 1);
        *(stack - 1) = *(stack - 2);
        *(stack - 2) = temp;
        DISPATCH();
      }
      OP(CALL) {
        arg = GETARG();
        NEXT(1);
        a = --stack; // Pop callable
        if (is_cfunc_p(a)) {
          temp = create_nil();
          frame->stack_top = stack - arg;
          a->c(snap, stack - arg, arg, &temp);
          stack -= arg; // Pop arguments
          if (temp.type == STYPE_ERR) {
            *stack++ = as_err(temp)->msg;
            *stack++ = as_err(temp)->err;
            goto raise;
          }
          *stack++ = temp;
        } else if (is_code_p(a)){
          code = (SCode*)a->o;
          if (arg != code->num_args) {
            RUNTIME_ERROR("snap_arity_error", "Invalid number of arguments");
          }
          frame->pc = pc;
          frame->stack_top = stack - arg; // Pop arguments
          push_frame(snap, code, frame->stack_top, NULL);
          goto new_frame;
        } else if (is_closure_p(a)){
          closure = (SClosure*)a->o;
          if (arg != closure->code->num_args) {
            RUNTIME_ERROR("snap_arity_error", "Invalid number of arguments");
          }
          frame->pc = pc;
          frame->stack_top = stack - arg; // Pop arguments
          push_frame(snap, closure->code, frame->stack_top, closure);
          goto new_frame;
        } else {
          RUNTIME_ERROR("snap_type_error", "Value is not callable");
        }
        DISPATCH();
      }
      OP(CLOSURE) {
        NEXT(1);
        a = --stack;
        if (is_code_p(a)) {
          int i;
          SClosure* enclosed = snap_closure_new(snap, as_code_p(a), frame->enclosed);
          SArr* descs = enclosed->code->closed_descs;
          for (i = 0; i < descs->len; ++i) {
            SClosedDesc* desc = as_closed_desc(descs->data[i]);
            SnapClosed* closed = &enclosed->closed[i];
            if (desc->islocal) {
              closed->value = &locals[desc->index];
            } else {
              closed->value = closure->closed[desc->index].value;
            }
          }
          frame->enclosed = enclosed;
          *stack++ = create_obj(enclosed);
        } else {
          RUNTIME_ERROR("snap_type_error", "Value is not callable");
        }
        DISPATCH();
      }
      OP(RETURN) {
        while (frame->enclosed != NULL) {
          int i;
          SClosure* enclosed = frame->enclosed;
          SArr* descs = enclosed->code->closed_descs;
          for (i = 0; i < descs->len; ++i) {
            SnapClosed* closed = &enclosed->closed[i];
            closed->closed = *closed->value;
            closed->value = &closed->closed;
          }
          frame->enclosed = enclosed->next;
        }
        if (snap->frames.vsize > 1) {
          SnapFrame* frame_prev = &mvec_back(&snap->frames) - 1;
          *frame_prev->stack_top++ = *--stack;
          mvec_pop(&snap->frames);
          goto new_frame;
        }
        return *--stack;
      }
      OP(JUMP) {
        arg = GETJARG();
        NEXT(arg);
        DISPATCH();
      }
      OP(JUMP_FALSE) {
        a = --stack;
        if (is_bool_p(a) && !a->b) {
          arg = GETJARG();
          NEXT(arg);
        } else {
          NEXT(1);
        }
        DISPATCH();
      }
      OP(JUMP_FALSE_OR_POP) {
        a = stack - 1;
        if (is_bool_p(a) && !a->b) {
          arg = GETJARG();
          NEXT(arg);
        } else {
          --stack;
          NEXT(1);
        }
        DISPATCH();
      }
      OP(JUMP_TRUE) {
        a = --stack;
        if (!is_bool_p(a) || a->b) {
          arg = GETJARG();
          NEXT(arg);
        } else {
          NEXT(1);
        }
        DISPATCH();
      }
      OP(JUMP_TRUE_OR_POP) {
        a = stack - 1;
        if (!is_bool_p(a) || a->b) {
          arg = GETJARG();
          NEXT(arg);
        } else {
          --stack;
          NEXT(1);
        }
        DISPATCH();
      }
      OP(BLOCK) {
        // TODO) Handle too many blocks
        arg = GETJARG();
        block = &frame->blocks[frame->blocks_top++];
        block->insts_offset = (pc - code->insts) + arg;
        block->stack_offset = stack - snap->stack;
        NEXT(1);
        DISPATCH();
      }
      OP(END_BLOCK) {
        NEXT(1);
        --frame->blocks_top;
        DISPATCH();
      }
      OP(RAISE) {
        goto raise;
      }
      OP(NOT) {
        NEXT(1);
        a = --stack;
        *stack++ = create_bool(is_bool_p(a) && !a->b);
        DISPATCH();
      }
      OP(ADD) { NEXT(1); binary_op(add, add_iop, add_fop) DISPATCH(); }
      OP(SUB) { NEXT(1); binary_op(sub, sub_iop, sub_fop) DISPATCH(); }
      OP(MUL) { NEXT(1); binary_op(mul, mul_iop, mul_fop) DISPATCH(); }
      OP(DIV) { NEXT(1); binary_op(div, div_iop, div_fop) DISPATCH(); }
      OP(MOD) { NEXT(1); binary_op(mod, mod_iop, mod_fop) DISPATCH(); }
      OP(LESS_THAN)      { NEXT(1); compare_op(<) DISPATCH();  }
      OP(GREATER_THAN)   { NEXT(1); compare_op(>) DISPATCH();  }
      OP(LESS_EQUALS)    { NEXT(1); compare_op(<=) DISPATCH(); }
      OP(GREATER_EQUALS) { NEXT(1); compare_op(>=) DISPATCH(); }
      OP(EQUALS)         { NEXT(1); compare_op(==) DISPATCH(); }
#ifndef INDIRECT_DISPATCH
    } }
#endif

#undef RUNTIME_ERROR

  return create_nil();

raise:
  while (snap->frames.vsize > 0) {
    frame = &mvec_back(&snap->frames);
    if (frame->blocks_top > 0) break;
    mvec_pop(&snap->frames);
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
                                 *(stack - 1),
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
  if (setjmp(snap->jmp) > 0) {
    result = create_obj(snap->cause);
    goto err;
  } else {
    while (parse(snap, &lex, &expr)) {
      snap_print(expr);
      printf("\n");
      if (!first)  insts_append(snap, code_gen, POP);
      first = false;
      compile(snap, code_gen, expr);
    }
  }
  insts_append(snap, code_gen, RETURN);

  code = anchor_code(snap_code_new(snap, code_gen));
  print_code(code, 0);
  push_frame(snap, code, snap->stack, NULL);
  result = exec(snap);
  snap_release(snap);

err:
  snap_release(snap);
  return result;
}

SObject* snap_anchor(Snap* snap, SObject* obj) {
  *mvec_push(&snap->anchors, SObject*) = obj;
  return obj;
}

void snap_release(Snap* snap) {
  mvec_pop(&snap->anchors);
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

static int compile_expr_list(Snap* snap, SCodeGen* code_gen, bool pop, SCons* sexpr);
static void compile_sexpr(Snap* snap, SCodeGen* code_gen, SCons* sexpr);

static void raise(Snap* snap, const char* err, const char* format, ...) {
  char msg[256];
  va_list args;
  va_start(args, format);
  vsnprintf(msg, sizeof(msg), format, args);
  va_end(args);
  snap->cause = snap_err_new_format(snap, err, msg);
  longjmp(snap->jmp, 1);
}

#if 0
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

static void parse_sub_sexpr(Snap* snap, SnapLex* lex, SCons* sexpr) {
  sexpr->first = parse_sexpr(snap, lex, snap_lex_next_token(lex));
  parse_expr_list(snap, lex, sexpr);
}

static void parse_call(Snap* snap, SnapLex* lex, SCons* sexpr) {
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
  SArr* params;

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

  params = snap_arr_new(snap, len);
  sexpr->first = create_obj(params);

  for (i = 0; i < len; ++i) {
    snap_lex_next_token(lex);
    params->data[i] = create_obj(snap_sym_new(snap, lex->val));
  }
  snap_lex_next_token(lex); /* Consume ']' */

  parse_expr_list(snap, lex, sexpr);
}

static void parse_loop_or_let(Snap* snap, SnapLex* lex, int loop_or_let, SCons* sexpr) {
  int token;
  int i, len = 0;
  const char* p;
  SArr* params;

  sexpr->first = create_form(loop_or_let);
  sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));

  if (snap_lex_next_token(lex) != '[') {
    raise(snap, "Expected parameter list at %d", lex->line);
  }

  p = lex->p;
  token = snap_lex_next_token(lex);
  do {
    int count = 0;
    if (token != '[') {
      raise(snap, "Expected parameter at %d", lex->line);
    }
    if (snap_lex_next_token(lex) != TK_ID) {
      raise(snap, "Expected id for parameter at %d", lex->line);
    }
    while ((token = snap_lex_next_token(lex)) != TK_EOF) {
      if (token == '[') {
        count++;
      } else if (token == ']') {
        if (count > 0) {
          count--;
          token = snap_lex_next_token(lex);
        } else {
          token = snap_lex_next_token(lex);
          break;
        }
      }
    }
    len++;
  } while(token == '[');

  if (token != ']') {
    raise(snap, "Expected ']' to terminate parameter list at %d", lex->line);
  }
  lex->p = p;

  params = snap_arr_new(snap, len);
  sexpr->first = create_obj(params);

  for (i = 0; i < len; ++i) {
    SArr* param = snap_arr_new(snap, 2);
    params->data[i] = create_obj(param);
    snap_lex_next_token(lex); /* Consume '[' */
    snap_lex_next_token(lex); /* Consume TK_ID */
    param->data[0] = create_obj(snap_sym_new(snap, lex->val));
    param->data[1] = parse_expr(snap, lex, snap_lex_next_token(lex));
    snap_lex_next_token(lex); /* Consume ']' */
  }
  snap_lex_next_token(lex); /* Consume ']' */

  parse_expr_list(snap, lex, sexpr);
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
  int count;

  sexpr->first = create_form(TK_RAISE);

  count = parse_expr_list(snap, lex, sexpr);
  if (count > 2) {
    raise(snap, "Too many arguments for raise at %d", lex->line);
  }

  if (count == 1) {
    sexpr = as_cons(sexpr->rest = create_obj(snap_cons_new(snap)));
    sexpr->first = create_nil();
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
    case '(': parse_sub_sexpr(snap, lex, sexpr); break;
    case TK_ID: parse_call(snap, lex, sexpr); break;
    case TK_DO: parse_cons(snap, lex, token, sexpr); break;
    case TK_DEF: parse_def(snap, lex, sexpr); break;
    case TK_IF: parse_if(snap, lex, sexpr); break;
    case TK_FN: parse_fn(snap, lex, sexpr); break;
    case TK_LET: case TK_LOOP: parse_loop_or_let(snap, lex, token, sexpr); break;
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
  snap_release(snap);
  return create_obj(sexpr);
}
#endif

static SValue parse_expr(Snap* snap, SnapLex* lex, int token);

static SValue parse_sexpr(Snap* snap, SnapLex* lex) {
  SCons* sexpr = anchor_cons(snap_cons_new(snap));
  int token = snap_lex_next_token(lex);
  if (token != ')') {
    SCons** curr;
    sexpr->first = parse_expr(snap, lex, token);
    curr = (SCons**)&sexpr->rest.o;
    while ((token = snap_lex_next_token(lex)) != ')') {
      (*curr) = snap_cons_new(snap);
      (*curr)->first = parse_expr(snap, lex, token);
      curr = (SCons**)&(*curr)->rest.o;
    }
  }
  snap_release(snap);
  if (token != ')') {
    raise(snap,
          "snap_parse_error",
          "Expected ')' to terminate s-expression at %d", lex->line);
  }
  return create_obj(sexpr);
}

static void parse_skip_seq(const int open, const int close, SnapLex* lex) {
  int count = 1;
  int token;
  do {
    token = snap_lex_next_token(lex);
    if (token == open) count++;
    if (token == close) count--;
  } while (count > 0 &&
           token != TK_EOF &&
           token != TK_TOO_BIG &&
           token != TK_INVALID);
}

static SValue parse_arr(Snap* snap, SnapLex* lex) {
  int token, len = 0, count = 0;
  SArr* arr;

  const char* p = lex->p;
  while ((token = snap_lex_next_token(lex)) != ']') {
    switch (token) {
      case '[':
        parse_skip_seq('[', ']', lex);
        ++len;
        break;
      case '(':
        parse_skip_seq('(', ')', lex);
        ++len;
        break;
      case '\'':
        // Don't count
        break;
      default:
        ++len;
        break;
    }
  }
  lex->p = p;

  arr = anchor_arr(snap_arr_new(snap, len));
  while ((token = snap_lex_next_token(lex)) != ']') {
    arr->data[count++] = parse_expr(snap, lex, token);
  }

  if (token != ']') {
    raise(snap,
          "snap_parse_error",
          "Expected ']' to terminate array at %d", lex->line);
  }

  snap_release(snap);
  return create_obj(arr);
}

static SValue parse_quote(Snap* snap, SnapLex* lex) {
  SCons* sexpr = anchor_cons(snap_cons_new(snap));
  sexpr->first = create_form(TK_QUOTE);
  sexpr->rest = create_obj(snap_cons_new(snap));
  as_cons(sexpr->rest)->first = parse_expr(snap, lex, snap_lex_next_token(lex));
  snap_release(snap);
  return create_obj(sexpr);
}

static SValue parse_expr(Snap* snap, SnapLex* lex, int token) {
  switch (token) {
    case '(':
      return parse_sexpr(snap, lex);
    case '[':
      return parse_arr(snap, lex);
    case '\'':
      return parse_quote(snap, lex);
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
    case TK_BEGIN:
    case TK_DEF:
    case TK_ELLIPSIS:
    case TK_IF:
    case TK_FN:
    case TK_LET:
    case TK_LOOP:
    case TK_QUOTE:
    case TK_RECUR:
    case TK_SET:
    case TK_RAISE: case TK_TRY: case TK_CATCH:
    case TK_ADD: case TK_SUB: case TK_MUL: case TK_DIV: case TK_MOD:
    case TK_LT: case TK_LE: case TK_GT: case TK_GE: case TK_EQ:
    case TK_NOT: case TK_AND: case TK_OR:
      return create_form(token);
    case TK_EOF:
      raise(snap,
            "snap_parse_error",
            "Premature end of file on line %d", lex->line);
      break;
    case TK_TOO_BIG:
      raise(snap,
            "snap_parse_error",
            "Literal or identifier is too big on line %d", lex->line);
      break;
    case TK_INVALID:
    default:
      raise(snap,
            "snap_parse_error",
            "Invalid token on line %d", lex->line);
      break;
  }
  return create_nil();
}

static bool parse(Snap* snap, SnapLex* lex, SValue* result) {
  int token = snap_lex_next_token(lex);
  if (TK_EOF == token) return false;
  *result = parse_expr(snap, lex, token);
  return true;
}

static void load_constant(Snap* snap, SCodeGen* code_gen, SValue constant);
static void load_variable(Snap* snap, SCodeGen* code_gen, SSymStr* sym);

static void compile(Snap* snap, SCodeGen* code_gen, SValue expr) {
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
    default:
      // error
      raise(snap, "snap_compile_error", "Unexpected type");
      break;
  }
}

static int get_or_add_index(SnapHash* hash, SValue key) {
  int index;
  SValue* val = snap_hash_get(hash, key);
  if (val) {
    index = val->i;
  } else {
    index = hash->hcount;
    snap_hash_put(hash, key, create_int(index));
  }
  return index;
}

static int find_local(SCodeGen* code_gen, SSymStr* sym) {
  SScope* scope = code_gen->scope;
  SValue key = create_obj(sym);
  while (scope) {
    SValue* val = snap_hash_get(&scope->local_names, key);
    if (val) return val->i;
    scope = scope->up;
  }
  return -1;
}

int add_local(Snap* snap, SCodeGen* code_gen, SSymStr* sym) {
  SScope* scope = code_gen->scope;
  SScope* curr = scope;
  SValue key = create_obj(sym);
  int index = 0;
  if (snap_hash_get(&code_gen->scope->local_names, key)) {
    raise(snap, "Variable '%s' already defined in current scope", sym->data);
  }
  while (curr) {
    index += curr->local_names.hcount;
    curr = curr->up;
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
  int index = code_gen->global_names.hcount;
  snap_hash_put(&code_gen->global_names, key, create_int(index));
  return index;
}

static void load_constant(Snap* snap, SCodeGen* code_gen, SValue constant) {
  int index = get_or_add_index(&code_gen->constants, constant);
  SInst* inst = insts_append(snap, code_gen, LOAD_CONSTANT);
  inst->arg = index;
  inst->arg_data = constant;
}

static int find_or_add_closed(Snap* snap, SCodeGen* code_gen, SSymStr* sym) {
  int index;
  SValue key = create_obj(sym);
  SCodeGen* curr = code_gen->up;
  SValue* val = snap_hash_get(&code_gen->closed_names, key);
  if (val) {
    return val->i;
  }
  if (curr) {
    index = find_local(curr, sym);
    if (index >= 0) {
      snap_hash_put(&code_gen->closed_names, key, create_int(code_gen->closed_descs.vsize));
      *mvec_push(&code_gen->closed_descs, SValue)
          = create_obj(snap_closed_desc_new(snap, true, index, sym));
      return code_gen->closed_descs.vsize - 1;
    }

    index = find_or_add_closed(snap, curr, sym);
    if (index >= 0) {
      snap_hash_put(&code_gen->closed_names, key, create_int(code_gen->closed_descs.vsize));
      *mvec_push(&code_gen->closed_descs, SValue)
          = create_obj(snap_closed_desc_new(snap, false, index, sym));
      return code_gen->closed_descs.vsize - 1;
    }
  }
  return -1;
}

static void load_variable(Snap* snap, SCodeGen* code_gen, SSymStr* sym) {
  int index = find_local(code_gen, sym);
  if (index >= 0) {
    SInst* inst = insts_append(snap, code_gen, LOAD_LOCAL);
    inst->arg = index;
    inst->arg_data = create_obj(sym);
    return;
  }
  index = find_or_add_closed(snap, code_gen, sym);
  if (index >= 0) {
    SInst* inst = insts_append(snap, code_gen, LOAD_CLOSED);
    inst->arg = index;
    inst->arg_data = create_obj(sym);
    return;
  }
  {
    int index = get_or_add_index(&code_gen->global_names, create_obj(sym));
    SInst* inst = insts_append(snap, code_gen, LOAD_GLOBAL);
    inst->arg = index;
    inst->arg_data = create_obj(sym);
  }
}

static void store_variable(Snap* snap, SCodeGen* code_gen, SSymStr* sym) {
  int index = find_local(code_gen, sym);
  if (index >= 0) {
    SInst* inst = insts_append(snap, code_gen, STORE_LOCAL);
    inst->arg = index;
    inst->arg_data = create_obj(sym);
    return;
  }
  index = find_or_add_closed(snap, code_gen, sym);
  if (index >= 0) {
    SInst* inst = insts_append(snap, code_gen, STORE_CLOSED);
    inst->arg = index;
    inst->arg_data = create_obj(sym);
    return;
  }
  {
    int index = get_or_add_index(&code_gen->global_names, create_obj(sym));
    SInst* inst = insts_append(snap, code_gen, STORE_GLOBAL);
    inst->arg = index;
    inst->arg_data = create_obj(sym);
  }
}

static void compile_def(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  SSymStr* sym;
  if (!sexpr || !is_sym(sexpr->first)) {
    raise(snap,
          "snap_compile_error",
          "Expected id for first argument of define");
  }
  if (!as_cons(sexpr->rest)) {
    raise(snap,
          "snap_compile_error",
          "Expected expression");
  }
  if (!is_empty(as_cons(sexpr->rest)->rest)) {
    raise(snap,
          "snap_compile_error",
          "No more expressions expected");
  }
  sym = as_sym(sexpr->first);
  compile(snap, code_gen, as_cons(sexpr->rest)->first);
  if (code_gen->scope) {
    SInst* inst = insts_append(snap, code_gen, STORE_LOCAL);
    inst->arg = add_local(snap, code_gen, sym);
    inst->arg_data = create_obj(sym);
  } else {
    SInst* inst = insts_append(snap, code_gen, STORE_GLOBAL);
    inst->arg = add_global(code_gen, sym);
    inst->arg_data = create_obj(sym);
  }
  insts_append(snap, code_gen, LOAD_NIL); /* TODO: Hack */
}

static void compile_if(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  SInst* cond_jump, * jump;

  if (!sexpr || !as_cons(sexpr->rest) || !as_cons(as_cons(sexpr->rest)->rest)) {
    raise(snap,
          "snap_compile_error",
          "Expected expressions");
  }

  if (!is_empty(as_cons(as_cons(sexpr->rest)->rest)->rest)) {
    raise(snap,
          "snap_compile_error",
          "No more expressions expected");
  }

  compile(snap, code_gen, sexpr->first);
  cond_jump = insts_append(snap, code_gen, JUMP_FALSE);

  /* True */
  compile(snap, code_gen, as_cons(sexpr->rest)->first);
  jump = insts_append(snap, code_gen, JUMP);
  jump_arg_init(&cond_jump->jump_arg, 1, code_gen->insts.prev);

  /* False */
  compile(snap, code_gen, as_cons(as_cons(sexpr->rest)->rest)->first);
  jump_arg_init(&jump->jump_arg, 1, code_gen->insts.prev);
}

static void compile_fn(Snap* snap, SCodeGen* up, SCons* sexpr) {
  int i;
  SArr* args;
  SCodeGen* code_gen;

  if (!sexpr || !is_arr(sexpr->first)) {
    raise(snap,
          "snap_compile_error",
          "Expected parameter list");
  }

  if (!as_cons(sexpr->rest)) {
    raise(snap,
          "snap_compile_error",
          "Expected expressions");
  }

  args = as_arr(sexpr->first);
  code_gen = anchor_code_gen(snap_code_gen_new(snap, up));
  code_gen->scope = snap_scope_new(snap, NULL);

  for (i = 0; i < args->len; ++i) {
    SSymStr* sym = as_sym(args->data[i]);
    *mvec_push(&code_gen->param_names, SValue) = create_obj(sym);
    add_local(snap, code_gen, sym);
  }

  compile_expr_list(snap, code_gen, true, as_cons(sexpr->rest));
  insts_append(snap, code_gen, RETURN);

  load_constant(snap, up, create_obj(snap_code_new(snap, code_gen)));
  if (code_gen->closed_descs.vsize > 0) {
    insts_append(snap, up, CLOSURE);
  }

  code_gen->scope = code_gen->scope->up;
  snap_release(snap);
}

static void compile_arith_expr(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  int i;
  int opcode;
  int count;
  if (!as_cons(sexpr->rest) || !as_cons(as_cons(sexpr->rest)->rest)) {
    raise(snap, "snap_compile_error", "Expected expressions");
  }
  opcode = (sexpr->first.i - TK_ADD) + ADD;
  count = compile_expr_list(snap, code_gen, false, as_cons(sexpr->rest));
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

static void compile_cond_not_expr(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  compile(snap, code_gen, sexpr->first);
  insts_append(snap, code_gen, NOT);
}

static void compile_cond_and_or_expr_inner(Snap* snap, SCodeGen* code_gen, int opcode, SCons* sexpr) {
  SInst* jump_false = NULL;
  SCons* curr = as_cons(sexpr->rest);
  compile(snap, code_gen, sexpr->first);

  if (curr) {
    jump_false = insts_append(snap, code_gen, opcode);
    compile_cond_and_or_expr_inner(snap, code_gen, opcode, curr);
  }

  if (jump_false) {
    jump_arg_init(&jump_false->jump_arg, 1, code_gen->insts.prev);
  }
}

static void compile_cond_and_or_expr(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  int token = sexpr->first.i;
  SCons* curr = as_cons(sexpr->rest);

  if (curr) {
    compile_cond_and_or_expr_inner(snap, code_gen,
                                   token == TK_AND ? JUMP_FALSE_OR_POP : JUMP_TRUE_OR_POP,
                                   curr);
  } else {
    load_constant(snap, code_gen, create_bool(token == TK_AND));
  }
}

static void compile_call(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  int count = compile_expr_list(snap, code_gen, false, as_cons(sexpr->rest));
  load_variable(snap, code_gen, as_sym(sexpr->first));
  insts_append(snap, code_gen, CALL)->arg = count;
}

static void compile_sub_sexpr(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  int count = compile_expr_list(snap, code_gen, false, as_cons(sexpr->rest));
  compile_sexpr(snap, code_gen, as_cons(sexpr->first));
  insts_append(snap, code_gen, CALL)->arg = count;
}

static void compile_set(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  compile(snap, code_gen, as_cons(sexpr->rest)->first);
  store_variable(snap, code_gen, as_sym(sexpr->first));
  insts_append(snap, code_gen, LOAD_NIL); /* TODO: Hack */
}

static void compile_loop_or_let(Snap* snap, SCodeGen* code_gen, int loop_or_let, SCons* sexpr) {
  int i;
  SArr* args;

  if (!sexpr || !is_arr(sexpr->first)) {
    raise(snap, "snap_compile_error", "Expected binding array");
  }

  if (!as_cons(sexpr->rest)) {
    raise(snap, "snap_compile_error", "Expected expressions");
  }

  args = as_arr(sexpr->first);

  for (i = 0; i < args->len; ++i) {
    if (!is_arr(args->data[i])) {
      raise(snap, "snap_compile_error", "Expected binding pair");
    }
    if (as_arr(args->data[i])->len != 2) {
      raise(snap, "snap_compile_error", "Invalid number of elements in binding pair");
    }
    if (!is_sym(as_arr(args->data[i])->data[0])) {
      raise(snap, "snap_compile_error", "Expected an identifier for the first element of binding pair");
    }
  }

  for (i = 0; i < args->len; ++i) {
    SArr* arg = as_arr(args->data[i]);
    compile(snap, code_gen, arg->data[1]);
  }

  code_gen->scope = snap_scope_new(snap, code_gen->scope);

  for (i = 0; i < args->len; ++i) {
    SArr* arg = as_arr(args->data[i]);
    SSymStr* sym = as_sym(arg->data[0]);
    if (loop_or_let == TK_LOOP) {
      *mvec_push(&code_gen->scope->param_names, SValue) = create_obj(sym);
    }
    add_local(snap, code_gen, sym);
  }

  for (i = args->len - 1; i >= 0; --i) {
    SArr* arg = as_arr(args->data[i]);
    int index = get_or_add_index(&code_gen->scope->local_names, arg->data[0]);
    SInst* inst = insts_append(snap, code_gen, STORE_LOCAL);
    inst->arg = index;
    inst->arg_data = arg->data[0];
  }

  code_gen->scope->top = code_gen->insts.prev;

  compile_expr_list(snap, code_gen, true, as_cons(sexpr->rest));

  code_gen->scope = code_gen->scope->up;
}

static void compile_recur(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  int i, count = 0;
  SCons* curr;
  SnapVec* param_names;

  if (code_gen->scope && code_gen->scope->param_names.vsize > 0) {
    param_names = &code_gen->scope->param_names;
  } else {
    param_names = &code_gen->param_names;
  }

  for (curr = sexpr; curr != NULL; curr = as_cons(curr->rest)) {
    if (count >= param_names->vsize) {
      raise(snap, "snap_compile_error", "Invalid number of arguments");
    }
    if (!is_sym(curr->first) ||
        snap_compare(&curr->first, &param_names->vitems[count]) != 0) {
      compile(snap, code_gen, curr->first);
    }
    count++;
  }

  for (i = count - 1; i >= 0; --i) {
    int j;
    for (j = 0, curr = sexpr; j < i; ++j, curr = as_cons(curr->rest)) { }
    if (!is_sym(curr->first) ||
        snap_compare(&curr->first, &param_names->vitems[i]) != 0) {
      int index = get_or_add_index(&code_gen->scope->local_names, param_names->vitems[i]);
      SInst* inst = insts_append(snap, code_gen, STORE_LOCAL);
      inst->arg = index;
      inst->arg_data = param_names->vitems[i];
    }
  }

  if (code_gen->scope && code_gen->scope->param_names.vsize > 0) {
    jump_arg_init(&insts_append(snap, code_gen, JUMP)->jump_arg, -1, code_gen->scope->top->next);
  } else {
    jump_arg_init(&insts_append(snap, code_gen, JUMP)->jump_arg, -1, code_gen->insts.next);
  }
}

static void compile_catch(Snap* snap, SCodeGen* code_gen,
                          SCons* sexpr, SCons* next_catch) {
  bool empty_catch = true;
  SInst* jump_false = NULL;
  MList* jump_to = NULL;
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
    add_local(snap, code_gen, as_sym(arr->data[0]));
    add_local(snap, code_gen, as_sym(arr->data[1]));
    store_variable(snap, code_gen, as_sym(arr->data[0]));
    store_variable(snap, code_gen, as_sym(arr->data[1]));
  } else if (arr->len == 1) {
    insts_append(snap, code_gen, POP);
    add_local(snap, code_gen, as_sym(arr->data[0]));
    store_variable(snap, code_gen, as_sym(arr->data[0]));
  } else {
    insts_append(snap, code_gen, POP);
    insts_append(snap, code_gen, POP);
  }

  compile_expr_list(snap, code_gen, true, as_cons(sexpr->rest));
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
}

static void compile_raise(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  compile(snap, code_gen, as_cons(sexpr->rest)->first);
  compile(snap, code_gen, sexpr->first);
  insts_append(snap, code_gen, RAISE);
}

static void compile_sexpr(Snap* snap, SCodeGen* code_gen, SCons* sexpr) {
  switch (sexpr->first.type) {
    case STYPE_SYM:
      compile_call(snap, code_gen, sexpr);
      break;
    case STYPE_CONS:
      compile_sub_sexpr(snap, code_gen, sexpr);
      break;
    case STYPE_FORM:
      switch (sexpr->first.i) {
        case TK_BEGIN:
          compile_expr_list(snap, code_gen, true, as_cons(sexpr->rest));
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
          compile_loop_or_let(snap, code_gen, TK_LET, as_cons(sexpr->rest));
          break;
        case TK_LOOP:
          compile_loop_or_let(snap, code_gen, TK_LOOP, as_cons(sexpr->rest));
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
        case TK_NOT:
          compile_cond_not_expr(snap, code_gen, as_cons(sexpr->rest));
          break;
        case TK_AND: case TK_OR:
          compile_cond_and_or_expr(snap, code_gen, sexpr);
          break;
      }
      break;
    default:
      raise(snap, "snap_compile_error", "Expected id, special form or expr");
      break;
  }
}

static int compile_expr_list(Snap* snap, SCodeGen* code_gen, bool pop, SCons* sexpr) {
  SCons* curr;
  int count = 0;
  for (curr = sexpr; curr != NULL; curr = as_cons(curr->rest)) {
    if (pop && count > 0) insts_append(snap, code_gen, POP);
    compile(snap, code_gen, curr->first);
    count++;
  }
  return count;
}

static SInst* insts_append(Snap* snap, SCodeGen* code_gen, int opcode) {
  SInst* inst = snap_inst_new(snap, opcode);
  code_gen->insts_count++;
  mlist_append(&code_gen->insts, &inst->list);
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
        case TK_BEGIN: iprintf(indent, "begin"); break;
        case TK_DEF: iprintf(indent, "def"); break;
        case TK_ELLIPSIS: iprintf(indent, "..."); break;
        case TK_IF: iprintf(indent, "if"); break;
        case TK_FN: iprintf(indent, "fn"); break;
        case TK_LET: iprintf(indent, "let"); break;
        case TK_LOOP: iprintf(indent, "loop"); break;
        case TK_QUOTE: iprintf(indent, "quote"); break;
        case TK_RECUR: iprintf(indent, "recur"); break;
        case TK_SET: iprintf(indent, "set"); break;
        case TK_RAISE: iprintf(indent, "raise"); break;
        case TK_TRY: iprintf(indent, "try"); break;
        case TK_CATCH: iprintf(indent, "catch"); break;
        case TK_LT: iprintf(indent, "<"); break;
        case TK_NOT: iprintf(indent, "not"); break;
        case TK_AND: iprintf(indent, "and"); break;
        case TK_OR: iprintf(indent, "or"); break;
        case TK_ADD: iprintf(indent, "+"); break;
        case TK_SUB: iprintf(indent, "-"); break;
        case TK_MUL: iprintf(indent, "*"); break;
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
      iprintf(indent, "<err ");
      print_val(as_err(val)->err, 0);
      printf(" ");
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
    case STYPE_CLOSURE:
      iprintf(indent, "<closure object (%p)>:\n", val.o);
      print_closure((SClosure*)as_closure(val), indent + 1);
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
  if (code->closed_descs->len > 0) {
    iprintf(indent, "closed_descs: {\n");
    for (i = 0; i < code->closed_descs->len; ++i) {
      SClosedDesc* desc = (SClosedDesc*)code->closed_descs->data[i].o;
      iprintf(indent + 1, "%d => { name: %s, islocal: %s, index: %d }\n",
              i, desc->name->data, desc->islocal ? "true" : "false", desc->index);
    }
    iprintf(indent, "}\n");
  }
  iprintf(indent, "instructions: {\n");
  insts_dump(&code->insts_debug, indent);
  iprintf(indent, "}\n");
}

static void print_closure(SClosure* closure, int indent) {
  int i;
  iprintf(indent, "code: {\n");
  print_code(closure->code, indent + 1);
  iprintf(indent, "}\n");
  iprintf(indent, "closed: {\n");
  for (i = 0; i < closure->code->closed_descs->len; ++i) {
    iprintf(indent + 1, "%d => ", i);
    print_val(*closure->closed[i].value, 0);
    printf("\n");
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
      int jump = mlist_forward_count(&inst->list, inst->jump_arg.dest) + 1;
      for (i = 0; i < jump; ++i) {
        inst = mlist_entry(SInst, inst->list.next, list);
      }
      total += jump;
    } else {
      int jump = mlist_backward_count(&inst->list, inst->jump_arg.dest);
      for (i = 0; i < jump; ++i) {
        inst = mlist_entry(SInst, inst->list.prev, list);
      }
      total -= jump;
    }
  } while (inst->opcode == JUMP && prev_inst != inst);
  return total;
}

static void insts_remove(MList* insts, SInst* to_remove) {
  MList* pos;
  mlist_foreach(insts) {
    SInst* inst = mlist_entry(SInst, pos, list);
    switch (inst->opcode) {
      case JUMP:
      case JUMP_FALSE:
      case JUMP_FALSE_OR_POP:
      case JUMP_TRUE:
      case JUMP_TRUE_OR_POP:
        if (inst->jump_arg.dest == &to_remove->list) {
          inst->jump_arg.dest = to_remove->list.next;
        }
        break;
    }
  }
  mlist_remove(&to_remove->list);
}

static void insts_remove_dead_code(MList* insts) {
  MList* pos = insts->next;
  while (pos != insts) {
    SInst* inst = mlist_entry(SInst, pos, list);
    SInst* prev_inst;
    pos = pos->next;
    switch (inst->opcode) {
      case POP:
        assert(inst->list.prev != insts);
        prev_inst = mlist_entry(SInst, inst->list.prev, list);
        switch (prev_inst->opcode) {
          case LOAD_CONSTANT:
          case LOAD_GLOBAL:
          case LOAD_LOCAL:
          case LOAD_CLOSED:
          case LOAD_NIL:
            insts_remove(insts, prev_inst);
            insts_remove(insts, inst);
            break;
        }
        break;
      case STORE_GLOBAL:
        assert(inst->list.prev != insts);
        prev_inst = mlist_entry(SInst, inst->list.prev, list);
        if (prev_inst->opcode == LOAD_GLOBAL && prev_inst->arg == inst->arg) {
          insts_remove(insts, prev_inst);
          insts_remove(insts, inst);
        }
        break;
      case STORE_LOCAL:
        assert(inst->list.prev != insts);
        prev_inst = mlist_entry(SInst, inst->list.prev, list);
        if (prev_inst->opcode == LOAD_LOCAL && prev_inst->arg == inst->arg) {
          insts_remove(insts, prev_inst);
          insts_remove(insts, inst);
        }
        break;
    }
  }
}

static void insts_dump(MList* insts, int indent) {
  MList* pos;
  int i = 0;
  int jump;
  mlist_foreach(insts) {
    SInst* inst = mlist_entry(SInst, pos, list);
    switch(inst->opcode) {
      case LOAD_GLOBAL:
      case STORE_GLOBAL:
      case LOAD_LOCAL:
      case STORE_LOCAL:
      case LOAD_CLOSED:
      case STORE_CLOSED:
        iprintf(indent + 1, "%8d %-18s %-8d (%s)\n",
                i, opcode_name(inst->opcode), inst->arg, as_sym(inst->arg_data)->data);
        break;
      case LOAD_CONSTANT:
        if (inst->arg_data.type == STYPE_BOOL  || inst->arg_data.type == STYPE_INT ||
            inst->arg_data.type == STYPE_FLOAT || inst->arg_data.type == STYPE_STR ||
            inst->arg_data.type == STYPE_SYM || inst->arg_data.type == STYPE_KEY) {
          iprintf(indent + 1, "%8d %-18s %-8d (",
                  i, opcode_name(inst->opcode), inst->arg);
          print_val(inst->arg_data, 0);
          printf(")\n");
        } else {
          iprintf(indent + 1, "%8d %-18s %-8d (<%s at %p>)\n",
                  i, opcode_name(inst->opcode), inst->arg,
                  stype_name(inst->arg_data.type), inst->arg_data.o);
        }
        break;
      case CALL:
        iprintf(indent + 1, "%8d %-18s %-8d\n",
                i, opcode_name(inst->opcode), inst->arg);
        break;
      case LOAD_NIL:
      case POP:
      case DUP:
      case ROT2:
      case ROT3:
      case CLOSURE:
      case RETURN:
      case END_BLOCK:
      case RAISE:
      case ADD:
      case SUB:
      case MUL:
      case DIV:
      case MOD:
      case LESS_THAN:
      case LESS_EQUALS:
      case GREATER_THAN:
      case GREATER_EQUALS:
      case EQUALS:
      case NOT:
        iprintf(indent + 1, "%8d %-18s\n", i, opcode_name(inst->opcode));
        break;
      case JUMP:
      case JUMP_FALSE:
      case JUMP_FALSE_OR_POP:
      case JUMP_TRUE:
      case JUMP_TRUE_OR_POP:
      case BLOCK:
        jump = insts_calculate_jump(inst);
        iprintf(indent + 1, "%8d %-18s %-8d (jump to %d)\n", i,
                opcode_name(inst->opcode), jump, i + jump);
        break;
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

void builtin_isnil(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) {
    *result = create_obj(snap_err_new_format(snap, "snap_arity_error", "Invalid number of arguments"));
    return;
  }
  *result = create_bool(args[0].type == STYPE_NIL);
}

void snap_init(Snap* snap) {
  snap->stack_size = 64;
  snap->stack = (SValue*)malloc(snap->stack_size * sizeof(SValue));
  snap->stack_end = snap->stack + snap->stack_size;
  mvec_init(&snap->frames, SnapFrame, 32);
  mvec_init(&snap->anchors, SObject*, 32);
  snap->num_bytes_alloced = 0;
  snap->num_bytes_alloced_last_gc = 0;
  snap->all = NULL;
  snap->gray = NULL;
  snap->cause = NULL;
  snap_hash_init(&snap->globals);
  snap_hash_init(&snap->keywords);

  snap_def(snap, "print", create_cfunc(builtin_print));
  snap_def(snap, "println", create_cfunc(builtin_println));
  snap_def(snap, "nil?", create_cfunc(builtin_isnil));
}

void snap_destroy(Snap* snap) {
  SObject* obj = snap->all;
  while (obj) {
    SObject* temp = obj;
    obj = obj->gc_next;
    gc_free(snap, temp);
  }
  free((void*)snap->stack);
  mvec_destroy(&snap->frames);
  mvec_destroy(&snap->anchors);
  snap_hash_destroy(&snap->globals);
  snap_hash_destroy(&snap->keywords);
  assert(snap->num_bytes_alloced == 0);
}
