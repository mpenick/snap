#include "snap.h"
#include "snap_lex.h"

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// After a collection the next one happens once the heap has grown by
// GC_HEAP_GROWTH times the live size, and by at least GC_MIN_STEP bytes
#ifndef GC_HEAP_GROWTH
#define GC_HEAP_GROWTH 2
#endif

#ifndef GC_MIN_STEP
#define GC_MIN_STEP (1024 * 1024)
#endif

#define MAX_ARG ((1 << 23) - 1)
#define MAX_LOCALS 256
#define MAX_UPVALS 256

enum {
  UNMARKED,
  MARKED
};

#define OPCODE_MAP(XX) \
  XX(LOAD_GLOBAL) \
  XX(STORE_GLOBAL) \
  XX(LOAD_LOCAL) \
  XX(STORE_LOCAL) \
  XX(LOAD_CLOSED) \
  XX(STORE_CLOSED) \
  XX(LOAD_CONSTANT) \
  XX(LOAD_NIL) \
  XX(POP) \
  XX(DUP) \
  XX(ROT2) \
  XX(ROT3) \
  XX(CALL) \
  XX(CLOSURE) \
  XX(CLOSE_UPVALS) \
  XX(RETURN) \
  XX(JUMP) \
  XX(JUMP_FALSE) \
  XX(JUMP_FALSE_OR_POP) \
  XX(JUMP_TRUE_OR_POP) \
  XX(TRY) \
  XX(PANIC) \
  XX(ADD) \
  XX(SUB) \
  XX(MUL) \
  XX(DIV) \
  XX(MOD) \
  XX(LESS_THAN) \
  XX(LESS_EQUALS) \
  XX(GREATER_THAN) \
  XX(GREATER_EQUALS) \
  XX(EQUALS) \
  XX(NOT)

enum {
#define XX(opcode) opcode,
  OPCODE_MAP(XX)
#undef XX
};

#define INST(op, arg) (((uint32_t)(arg) << 8) | (uint32_t)(op))
#define INST_OP(inst) ((inst) & 0xFF)
#define INST_ARG(inst) ((int32_t)(inst) >> 8)

// Binary ops (ADD..EQUALS) can take their operands directly from a local or a
// constant, and arithmetic ops can store their result in a local. The arg
// holds an unsigned 8-bit destination then two signed 8-bit operands:
//   | dest:8 | left:8 | right:8 | op:8 |
// An operand of 0 is the stack, n > 0 is local n - 1 and n < 0 is constant
// -n - 1. A destination of 0 pushes and n > 0 stores in local n - 1.
#define is_binary_op(op) ((op) >= ADD && (op) <= EQUALS)
#define is_arith_op(op) ((op) >= ADD && (op) <= MOD)
#define OPERAND_DEST(inst) ((inst) >> 24)
#define OPERAND_LEFT(inst) ((int8_t)((inst) >> 16))
#define OPERAND_RIGHT(inst) ((int8_t)((inst) >> 8))
#define OPERANDS(dest, left, right) \
  ((int)(((uint32_t)(dest) << 16) | (((uint32_t)(left) & 0xFF) << 8) | ((uint32_t)(right) & 0xFF)))
#define MAX_OPERAND 127
#define MAX_DEST 255

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

#define compare(a, b) ((a) < (b) ? -1 : (a) > (b))

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

static int hash_chars(const char* data, size_t len) {
  return (int)mhash_fnv1a((const void*)data, len);
}

static int compare_chars(const char* data1, size_t len1, const char* data2, size_t len2) {
  if (len1 != len2) {
    return len1 < len2 ? -1 : 1;
  }
  return strncmp(data1, data2, len1);
}

static int compare_tempstring(const STempStr* ts, const SValue* val) {
  switch (snap_type(*val)) {
    case STYPE_SYM:
      return compare_chars(ts->data, ts->len, as_sym(*val)->data, as_sym(*val)->len);
    case STYPE_STR:
      return compare_chars(ts->data, ts->len, as_str(*val)->data, as_str(*val)->len);
    case STYPE_KEY:
      return compare_chars(ts->data, ts->len, as_key(*val)->data, as_key(*val)->len);
    default:
      return STYPE_TEMPSTR < snap_type(*val) ? -1 : 1;
  }
}

static int hash_pointer(void* ptr) {
  uintptr_t p = (uintptr_t)ptr;
  return (int)((p >> 4) | (p << (8 * sizeof(ptr) - 4)));
}

int snap_hash(const SValue* val) {
  SObject* obj = as_obj(*val);
  if (!is_obj(*val)) {
    return (int)(val->bits ^ (val->bits >> 32)); // Ints, nil, bools, forms and cfuncs
  }
  switch (obj->type) {
    case STYPE_FLOAT:
      return hash_float(((SFloat*)obj)->f);
    // Hashed by contents so interning can look them up with a STempStr
    case STYPE_SYM:
      return hash_chars(((SSym*)obj)->data, ((SSym*)obj)->len);
    case STYPE_STR:
      return hash_chars(((SStr*)obj)->data, ((SStr*)obj)->len);
    case STYPE_KEY:
      return hash_chars(((SKeyword*)obj)->data, ((SKeyword*)obj)->len);
    case STYPE_TEMPSTR:
      return hash_chars(((STempStr*)obj)->data, ((STempStr*)obj)->len);
    // The rest match snap_compare(): errors by kind, vectors and maps by contents
    case STYPE_ERR:
      return snap_hash(&((SErr*)obj)->err);
    case STYPE_VEC: {
      SVec* v = (SVec*)obj;
      unsigned h = 1;
      int i;
      for (i = 0; i < v->len; ++i) h = h * 31 + snap_hash(&v->data[i]);
      return (int)h;
    }
    case STYPE_MAP: {
      SMap* m = (SMap*)obj;
      unsigned h = 0;
      int i;
      for (i = 0; i < m->used; ++i) {
        SValue* e = &m->entries[2 * i];
        // Added, so the order doesn't matter
        if (e->bits != SV_DELETED) h += (unsigned)snap_hash(e) * 31 ^ snap_hash(e + 1);
      }
      return (int)h;
    }
    default:
      return hash_pointer(obj);
  }
}

int snap_compare(const SValue* val1, const SValue* val2) {
  int type1 = snap_type(*val1), type2 = snap_type(*val2);
  assert(type2 != STYPE_TEMPSTR);

  if (type1 == STYPE_TEMPSTR) {
    return compare_tempstring((STempStr*)as_obj(*val1), val2);
  } else if (type1 != type2) {
    return type1 < type2 ? -1 : 1;
  }

  switch (type1) {
    case STYPE_INT:
      return compare((SnapInt)val1->bits, (SnapInt)val2->bits);
    case STYPE_FLOAT:
      return compare(as_float(*val1), as_float(*val2));
    case STYPE_STR:
      return compare_chars(as_str(*val1)->data, as_str(*val1)->len,
                           as_str(*val2)->data, as_str(*val2)->len);
    case STYPE_ERR:
      return snap_compare(&as_err(*val1)->err, &as_err(*val2)->err);
    case STYPE_VEC: {
      // By contents, element by element
      SVec* v1 = as_vec(*val1), * v2 = as_vec(*val2);
      int i, c;
      for (i = 0; i < v1->len && i < v2->len; ++i) {
        if ((c = snap_compare(&v1->data[i], &v2->data[i])) != 0) return c;
      }
      return compare(v1->len, v2->len);
    }
    case STYPE_MAP: {
      // Equal when they have the same entries, otherwise ordered arbitrarily
      SMap* m1 = as_map(*val1), * m2 = as_map(*val2);
      int i;
      if (m1->len != m2->len) return compare(m1->len, m2->len);
      for (i = 0; i < m1->used; ++i) {
        SValue* e = &m1->entries[2 * i];
        SValue* v2;
        if (e->bits == SV_DELETED) continue;
        v2 = snap_map_get(m2, *e);
        if (!v2 || snap_compare(e + 1, v2) != 0) {
          return compare((uintptr_t)m1, (uintptr_t)m2);
        }
      }
      return 0;
    }
    default:
      // Everything else, including interned symbols and keywords, by identity
      return compare(val1->bits, val2->bits);
  }
}

static SValue exec(Snap* snap);

static void print_val(SValue val);
static void print_code(SCode* code, int indent);

static inline MHASH_FUNC(SnapHashEntry, SValue) {
  return snap_hash(&key);
}

static inline MHASH_EQUALS_FUNC(SnapHashEntry, SValue) {
  return snap_compare(&key1, &key2) == 0;
}

void snap_hash_init(SnapHash* hash) {
  mhash_init(hash, SnapHashEntry, create_undef(), snap_val(SV_DELETED), 0);
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

/* GC */

// Bytes owned by a code object outside of its own allocation
static size_t code_extra_bytes(SCode* code) {
  return code->insts_count * sizeof(SnapInst) +
         code->num_constants * sizeof(SValue) +
         code->num_upvals * sizeof(uint32_t);
}

#define map_bytes(cap) ((size_t)(cap) * 2 * (sizeof(SValue) + sizeof(int32_t)))

static void gc_free(Snap* snap, SObject* obj) {
  switch (obj->type) {
    case STYPE_FLOAT:
      snap->num_bytes_alloced -= sizeof(SFloat);
      break;
    case STYPE_SYM:
      snap->num_bytes_alloced -= (sizeof(SSym) + ((SSym*)obj)->len + 1);
      break;
    case STYPE_STR:
      snap->num_bytes_alloced -= (sizeof(SStr) + ((SStr*)obj)->len + 1);
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
    case STYPE_VEC:
      snap->num_bytes_alloced -= (sizeof(SVec) + ((SVec*)obj)->cap * sizeof(SValue));
      free(((SVec*)obj)->data);
      break;
    case STYPE_MAP:
      snap->num_bytes_alloced -= sizeof(SMap) + map_bytes(((SMap*)obj)->cap);
      free(((SMap*)obj)->entries);
      free(((SMap*)obj)->index);
      break;
    case STYPE_CODE:
      snap->num_bytes_alloced -= sizeof(SCode) + code_extra_bytes((SCode*)obj);
      free(((SCode*)obj)->insts);
      free(((SCode*)obj)->constants);
      free(((SCode*)obj)->upvals);
      break;
    case STYPE_CLOSURE:
      snap->num_bytes_alloced -= (sizeof(SClosure) + ((SClosure*)obj)->num_upvals * sizeof(SUpval*));
      break;
    case STYPE_UPVAL:
      snap->num_bytes_alloced -= sizeof(SUpval);
      break;
    default:
      assert(0 && "Not a GC object");
      break;
  }
  free(obj);
}

static void gc_mark(Snap* snap, SObject* obj) {
  if (!obj || obj->mark == MARKED) return;
  obj->mark = MARKED;
  switch (obj->type) {
    case STYPE_FLOAT:
    case STYPE_STR:
    case STYPE_KEY:
      break; // No children
    case STYPE_SYM:
    case STYPE_ERR:
    case STYPE_CONS:
    case STYPE_VEC:
    case STYPE_MAP:
    case STYPE_CODE:
    case STYPE_CLOSURE:
    case STYPE_UPVAL:
      *mvec_push(&snap->gray, SObject*) = obj;
      break;
    default:
      assert(0 && "Not a GC object");
      break;
  }
}

static void gc_mark_val(Snap* snap, SValue val) {
  if (is_obj(val)) gc_mark(snap, as_obj(val));
}

static void gc_mark_vals(Snap* snap, SValue* vals, int count) {
  int i;
  for (i = 0; i < count; ++i) {
    gc_mark_val(snap, vals[i]);
  }
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

static void gc_mark_children(Snap* snap, SObject* obj) {
  int i;
  switch (obj->type) {
    case STYPE_SYM:
      gc_mark_val(snap, ((SSym*)obj)->value);
      break;
    case STYPE_ERR:
      gc_mark_val(snap, ((SErr*)obj)->err);
      gc_mark_val(snap, ((SErr*)obj)->msg);
      break;
    case STYPE_CONS:
      gc_mark_val(snap, ((SCons*)obj)->first);
      gc_mark_val(snap, ((SCons*)obj)->rest);
      break;
    case STYPE_VEC:
      gc_mark_vals(snap, ((SVec*)obj)->data, ((SVec*)obj)->len);
      break;
    case STYPE_MAP:
      gc_mark_vals(snap, ((SMap*)obj)->entries, 2 * ((SMap*)obj)->used); // Deleted keys aren't objects
      break;
    case STYPE_CODE:
      gc_mark_vals(snap, ((SCode*)obj)->constants, ((SCode*)obj)->num_constants);
      break;
    case STYPE_CLOSURE:
      gc_mark(snap, (SObject*)((SClosure*)obj)->code);
      for (i = 0; i < ((SClosure*)obj)->num_upvals; ++i) {
        gc_mark(snap, (SObject*)((SClosure*)obj)->upvals[i]);
      }
      break;
    case STYPE_UPVAL:
      gc_mark_val(snap, *((SUpval*)obj)->value);
      break;
    default:
      assert(0 && "Not a valid gray object");
      break;
  }
}

static void gc_mark_fn_states(Snap* snap);

static void gc_collect(Snap* snap) {
  SObject** obj;
  SnapFrame* frame;
  SUpval* upval;

  mvec_foreach(&snap->anchors, obj) {
    gc_mark(snap, *obj);
  }

  // Interned symbols (and so all globals) and keywords live forever
  gc_mark_hash(snap, &snap->symbols);
  gc_mark_hash(snap, &snap->keywords);
  gc_mark(snap, (SObject*)snap->cause);

  // Each frame's [stack_base, stack_top) region is live (args, locals and
  // temporaries); the top frame's stack_top is synced before any allocation.
  mvec_foreach(&snap->frames, frame) {
    gc_mark(snap, (SObject*)frame->code);
    gc_mark(snap, (SObject*)frame->closure);
    gc_mark_vals(snap, frame->stack_base, frame->stack_top - frame->stack_base);
  }

  for (upval = snap->open_upvals; upval; upval = upval->next) {
    gc_mark(snap, (SObject*)upval);
  }

  gc_mark_fn_states(snap);

  while (snap->gray.vsize > 0) {
    gc_mark_children(snap, snap->gray.vitems[--snap->gray.vsize]);
  }

  obj = &snap->all;

  while (*obj) {
    if ((*obj)->mark == UNMARKED) {
      SObject* temp = *obj;
      *obj = (*obj)->gc_next;
      gc_free(snap, temp);
    } else {
      (*obj)->mark = UNMARKED;
      obj = &(*obj)->gc_next;
    }
  }

  snap->next_gc = snap->num_bytes_alloced * GC_HEAP_GROWTH;
  if (snap->next_gc < snap->num_bytes_alloced + GC_MIN_STEP) {
    snap->next_gc = snap->num_bytes_alloced + GC_MIN_STEP;
  }
}

static SObject* gc_new(Snap* snap, uint8_t type, size_t size) {
  SObject* obj;
  assert(size > sizeof(SObject));
  if (snap->num_bytes_alloced + size > snap->next_gc) {
    gc_collect(snap);
  }
  obj = (SObject*)malloc(size);
  obj->type = type;
  obj->mark = UNMARKED;
  obj->read_only = false;
  obj->gc_next = snap->all;
  snap->all = obj;
  snap->num_bytes_alloced += size;
  return obj;
}

/* Objects */

SValue create_float(Snap* snap, SnapFloat f) {
  SFloat* n = (SFloat*)gc_new(snap, STYPE_FLOAT, sizeof(SFloat));
  n->f = f;
  return create_obj(n);
}

static SStr* str_new(Snap* snap, const char* data, size_t len) {
  SStr* s = (SStr*)gc_new(snap, STYPE_STR, sizeof(SStr) + len + 1);
  s->len = len;
  memcpy(s->data, data, len);
  s->data[len] = '\0';
  return s;
}

SStr* snap_str_new(Snap* snap, const char* str) {
  return str_new(snap, str, strlen(str));
}

SStr* snap_str_new_vformat(Snap* snap, const char* format, va_list args) {
  char buf[256]; /* TODO: Dynamic buffer size */
  int len = vsnprintf(buf, sizeof(buf), format, args);
  if (len >= (int)sizeof(buf)) len = sizeof(buf) - 1;
  return str_new(snap, buf, len);
}

SStr* snap_str_new_format(Snap* snap, const char* format, ...) {
  SStr* s;
  va_list args;
  va_start(args, format);
  s = snap_str_new_vformat(snap, format, args);
  va_end(args);
  return s;
}

SSym* snap_sym_new(Snap* snap, const char* name) {
  size_t len = strlen(name);
  SValue* existing = snap_hash_get_str(&snap->symbols, name, len);
  SSym* s;
  if (existing) {
    return as_sym(*existing);
  }
  s = (SSym*)gc_new(snap, STYPE_SYM, sizeof(SSym) + len + 1);
  s->value = create_undef();
  s->len = len;
  memcpy(s->data, name, len + 1);
  snap_hash_put(&snap->symbols, create_obj(s), create_obj(s));
  return s;
}

SKeyword* snap_key_new(Snap* snap, const char* name) {
  size_t len = strlen(name);
  SValue* existing = snap_hash_get_str(&snap->keywords, name, len);
  SKeyword* k;
  if (existing) {
    return as_key(*existing);
  }
  k = (SKeyword*)gc_new(snap, STYPE_KEY, sizeof(SKeyword) + len + 1);
  k->len = len;
  memcpy(k->data, name, len + 1);
  k->id = snap->keywords.hcount;
  snap_hash_put(&snap->keywords, create_obj(k), create_obj(k));
  return k;
}

SErr* snap_err_new(Snap* snap, SValue err, SValue msg) {
  SErr* e = (SErr*)gc_new(snap, STYPE_ERR, sizeof(SErr));
  e->err = err;
  e->msg = msg;
  return e;
}

SErr* snap_err_new_format(Snap* snap, const char* err, const char* format, ...) {
  SStr* s;
  SKeyword* k;
  SErr* e;
  va_list args;
  va_start(args, format);
  s = (SStr*)snap_anchor(snap, (SObject*)snap_str_new_vformat(snap, format, args));
  va_end(args);
  k = snap_key_new(snap, err); // Interned, so already rooted
  e = snap_err_new(snap, create_obj(k), create_obj(s));
  snap_release(snap);
  return e;
}

SCons* snap_cons_new(Snap* snap) {
  SCons* c = (SCons*)gc_new(snap,  STYPE_CONS, sizeof(SCons));
  c->first = create_undef();
  c->rest = create_empty();
  return c;
}

SVec* snap_vec_new(Snap* snap, int cap) {
  SVec* v = (SVec*)gc_new(snap, STYPE_VEC, sizeof(SVec));
  v->len = 0;
  v->cap = cap;
  v->data = cap > 0 ? (SValue*)malloc(cap * sizeof(SValue)) : NULL;
  snap->num_bytes_alloced += cap * sizeof(SValue);
  return v;
}

// Doesn't allocate GC objects, so it never collects
void snap_vec_push(Snap* snap, SVec* vec, SValue val) {
  if (vec->len == vec->cap) {
    int cap = vec->cap > 0 ? vec->cap * 2 : 4;
    vec->data = (SValue*)realloc(vec->data, cap * sizeof(SValue));
    snap->num_bytes_alloced += (cap - vec->cap) * sizeof(SValue);
    vec->cap = cap;
  }
  vec->data[vec->len++] = val;
}

SMap* snap_map_new(Snap* snap) {
  SMap* m = (SMap*)gc_new(snap, STYPE_MAP, sizeof(SMap));
  m->len = m->used = m->cap = 0;
  m->entries = NULL;
  m->index = NULL;
  return m;
}

// Returns the key's entry, or -1. Linear probing; deleted entries keep their
// index slots so that probing continues past them.
static int map_find(SMap* map, SValue key) {
  int i, mask = 2 * map->cap - 1;
  if (map->cap == 0) return -1;
  for (i = snap_hash(&key) & mask; map->index[i] >= 0; i = (i + 1) & mask) {
    SValue* k = &map->entries[2 * map->index[i]];
    if (k->bits == key.bits || snap_compare(k, &key) == 0) return map->index[i];
  }
  return -1;
}

static void map_index(SMap* map, int entry) {
  int i, mask = 2 * map->cap - 1;
  for (i = snap_hash(&map->entries[2 * entry]) & mask; map->index[i] >= 0; i = (i + 1) & mask) {}
  map->index[i] = entry;
}

// Drops deleted entries and sizes the map to twice its live entries.
// Doesn't allocate GC objects, so it never collects.
static void map_rebuild(Snap* snap, SMap* map) {
  int i, len = 0, cap = 4;
  SValue* entries;
  while (cap < 2 * map->len) cap *= 2;
  entries = (SValue*)malloc(2 * cap * sizeof(SValue));
  for (i = 0; i < map->used; ++i) {
    if (map->entries[2 * i].bits != SV_DELETED) {
      entries[2 * len] = map->entries[2 * i];
      entries[2 * len + 1] = map->entries[2 * i + 1];
      len++;
    }
  }
  free(map->entries);
  free(map->index);
  snap->num_bytes_alloced = snap->num_bytes_alloced - map_bytes(map->cap) + map_bytes(cap);
  map->entries = entries;
  map->index = (int32_t*)malloc(2 * cap * sizeof(int32_t));
  memset(map->index, -1, 2 * cap * sizeof(int32_t));
  map->cap = cap;
  map->used = len;
  for (i = 0; i < len; ++i) map_index(map, i);
}

SValue* snap_map_get(SMap* map, SValue key) {
  int e = map_find(map, key);
  return e < 0 ? NULL : &map->entries[2 * e + 1];
}

// Doesn't allocate GC objects, so it never collects
void snap_map_put(Snap* snap, SMap* map, SValue key, SValue val) {
  int e = map_find(map, key);
  if (e < 0) {
    if (map->used == map->cap) map_rebuild(snap, map);
    e = map->used++;
    map->entries[2 * e] = key;
    map_index(map, e);
    map->len++;
  }
  map->entries[2 * e + 1] = val;
}

bool snap_map_del(SMap* map, SValue key) {
  int e = map_find(map, key);
  if (e < 0) return false;
  map->entries[2 * e] = snap_val(SV_DELETED);
  map->entries[2 * e + 1] = create_nil();
  map->len--;
  return true;
}

SClosure* snap_closure_new(Snap* snap, SCode* code) {
  int i;
  SClosure* cl = (SClosure*)gc_new(snap,
                                   STYPE_CLOSURE,
                                   sizeof(SClosure) +
                                   sizeof(SUpval*) *
                                   code->num_upvals);
  cl->code = code;
  cl->num_upvals = code->num_upvals;
  for (i = 0; i < cl->num_upvals; ++i) {
    cl->upvals[i] = NULL;
  }
  return cl;
}

SObject* snap_anchor(Snap* snap, SObject* obj) {
  *mvec_push(&snap->anchors, SObject*) = obj;
  return obj;
}

void snap_release(Snap* snap) {
  mvec_pop(&snap->anchors);
}

void snap_def(Snap* snap, const char* name, SValue val) {
  snap_sym_new(snap, name)->value = val;
}

void snap_def_cfunc(Snap* snap, const char* name, SCFunc cfunc) {
  snap_def(snap, name, create_cfunc(cfunc));
}

/* VM */

// Stack layout
// [args][locals][stack...]

// During call
// 1) [args and locals][stack...][calling args][function]
// 2) [args and locals][stack...][args and locals][stack...]
// 3) [args and locals][stack...][return value]

static void push_frame(Snap* snap, SCode* code, SValue* stack_base, SClosure* closure) {
  SnapFrame* frame;
  SValue* val;
  int needed = code->num_locals + code->max_stack_size;

  if (stack_base + needed >= snap->stack_end) {
    int offset = stack_base - snap->stack;
    int stack_size = snap->stack_size;
    SValue* stack_old = snap->stack;
    SValue* stack_new;
    SUpval* upval;

    while (offset + needed >= stack_size) {
      stack_size *= (stack_size < 4096 ? 4 : 2);
    }
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
    stack_base = stack_new + offset;
  }

  frame = mvec_push(&snap->frames, SnapFrame);
  frame->code = code;
  frame->closure = closure;
  frame->pc = code->insts;
  frame->stack_base = stack_base;
  frame->stack_top = stack_base + code->num_locals;

  // Non-argument locals must not be garbage when the GC scans the stack
  for (val = stack_base + code->num_args; val < frame->stack_top; ++val) {
    *val = create_nil();
  }
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

#define is_int_p(v) is_int(*(v))
#define is_num_p(v) (is_int_p(v) || is_float(*(v)))
#define num_p(v) (is_int_p(v) ? (SnapFloat)as_int(*(v)) : as_float(*(v)))

#define operand(n) ((n) > 0 ? &locals[(n) - 1] : &constants[-(n) - 1])

// Right operand first: when both come from the stack it's on top
#define binary_operands() \
  b = OPERAND_RIGHT(inst) ? operand(OPERAND_RIGHT(inst)) : --stack; \
  a = OPERAND_LEFT(inst) ? operand(OPERAND_LEFT(inst)) : --stack;

// Floats are allocated, so a float result is stored by saving the frame,
// calling this and reloading the frame. Keeping the VM's registers live across
// the call instead would spill them in every handler.
static void store_float(Snap* snap, SnapFloat f, int dest) {
  SnapFrame* frame = &mvec_back(&snap->frames);
  SValue val = create_float(snap, f);
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
  } else if (is_num_p(a) && is_num_p(b)) { \
    SnapFloat f = fop(num_p(a), num_p(b)); \
    frame->pc = pc; \
    frame->stack_top = stack; \
    store_float(snap, f, OPERAND_DEST(inst)); \
    goto new_frame; \
  } else { \
    RUNTIME_ERROR("snap_type_error", "Expected numbers"); \
  }

// A compare followed by JUMP_FALSE (as in every `(if (< ...) ...)`) takes the
// branch itself rather than pushing a bool for the next instruction. Tagged
// ints order the same as their values.
#define compare_op(op) \
  binary_operands() \
  if (is_int_p(a) && is_int_p(b)) { \
    cond = (SnapInt)a->bits op (SnapInt)b->bits; \
  } else if (is_num_p(a) && is_num_p(b)) { \
    cond = num_p(a) op num_p(b); \
  } else { \
    cond = snap_compare(a, b) op 0; \
  } \
  if (INST_OP(*pc) == JUMP_FALSE) { \
    arg = INST_ARG(*pc++); \
    if (!cond) pc += arg; \
  } else { \
    *stack++ = create_bool(cond); \
  }

// Integer ops on tagged words (2x + 1). They wrap on overflow, done unsigned
// to avoid undefined behavior. 63-bit division can't overflow an int64_t.
#define untag(x) ((SnapInt)(x) >> 1)
#define tag(x) (((uint64_t)(x) << 1) | 1)
#define add_iop(x, y) ((x) + (y) - 1)
#define sub_iop(x, y) ((x) - (y) + 1)
#define mul_iop(x, y) ((uint64_t)untag(x) * ((y) - 1) + 1)
#define div_iop(x, y) tag(untag(x) / untag(y))
#define mod_iop(x, y) tag(untag(x) % untag(y))

#define add_fop(x, y) ((x) + (y))
#define sub_fop(x, y) ((x) - (y))
#define mul_fop(x, y) ((x) * (y))
#define div_fop(x, y) ((x) / (y))
#define mod_fop(x, y) fmod(x, y)

#define int_zero_divisor() (is_int_p(b) && as_int(*b) == 0 && is_int_p(a))

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
        as_sym(constants[arg])->value = *--stack;
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
        *closure->upvals[arg]->value = *--stack;
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
      OP(CALL) {
        a = --stack; // Pop callable
        if (is_cfunc(*a)) {
          temp = create_nil();
          frame->stack_top = stack; // Keep the arguments rooted during the call
          ok = as_cfunc(*a)(snap, stack - arg, arg, &temp);
          stack -= arg; // Pop arguments
          if (!ok) {
            *stack++ = as_err(temp)->msg;
            *stack++ = as_err(temp)->err;
            goto panic;
          }
          *stack++ = temp;
        } else if (is_code(*a) || is_closure(*a)) {
          SClosure* cl = is_closure(*a) ? as_closure(*a) : NULL;
          SCode* c = cl ? cl->code : as_code(*a);
          if (arg != c->num_args) {
            RUNTIME_ERROR("snap_arity_error", "Invalid number of arguments");
          }
          frame->pc = pc;
          frame->stack_top = stack - arg; // Pop arguments
          push_frame(snap, c, frame->stack_top, cl);
          goto new_frame;
        } else {
          RUNTIME_ERROR("snap_type_error", "Value is not callable");
        }
        DISPATCH();
      }
      OP(CLOSURE) {
        int i;
        SCode* c = as_code(*(stack - 1));
        SClosure* cl;
        frame->stack_top = stack;
        cl = snap_closure_new(snap, c);
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
        if (snap->frames.vsize > 1) {
          SnapFrame* frame_prev = &mvec_back(&snap->frames) - 1;
          *frame_prev->stack_top++ = *--stack;
          mvec_pop(&snap->frames);
          goto new_frame;
        }
        temp = *--stack;
        mvec_pop(&snap->frames);
        return temp;
      }
      OP(JUMP) {
        pc += arg;
        DISPATCH();
      }
      OP(JUMP_FALSE) {
        if (is_falsy(*--stack)) pc += arg;
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
      OP(PANIC) {
        goto panic;
      }
      OP(NOT) {
        *(stack - 1) = create_bool(is_falsy(*(stack - 1)));
        DISPATCH();
      }
      OP(ADD) { binary_operands() arith_op(add_iop, add_fop) DISPATCH(); }
      OP(SUB) { binary_operands() arith_op(sub_iop, sub_fop) DISPATCH(); }
      OP(MUL) { binary_operands() arith_op(mul_iop, mul_fop) DISPATCH(); }
      OP(DIV) {
        binary_operands()
        if (int_zero_divisor()) {
          RUNTIME_ERROR("snap_arith_error", "Division by zero");
        }
        arith_op(div_iop, div_fop)
        DISPATCH();
      }
      OP(MOD) {
        binary_operands()
        if (int_zero_divisor()) {
          RUNTIME_ERROR("snap_arith_error", "Division by zero");
        }
        arith_op(mod_iop, mod_fop)
        DISPATCH();
      }
      OP(LESS_THAN)      { compare_op(<) DISPATCH();  }
      OP(GREATER_THAN)   { compare_op(>) DISPATCH();  }
      OP(LESS_EQUALS)    { compare_op(<=) DISPATCH(); }
      OP(GREATER_EQUALS) { compare_op(>=) DISPATCH(); }
      OP(EQUALS)         { compare_op(==) DISPATCH(); }
#ifndef INDIRECT_DISPATCH
    }
  }
#endif

#undef RUNTIME_ERROR

panic:
  // Stack: [... msg err]. Allocate the error while the stack is still rooted.
  frame->stack_top = stack;
  temp = create_obj(snap_err_new(snap, *(stack - 1), *(stack - 2)));
  close_upvals(snap, snap->stack);
  snap->frames.vsize = 0;
  return temp;
}

/* Parser */

static void raise(Snap* snap, const char* err, const char* format, ...) {
  char msg[256];
  va_list args;
  va_start(args, format);
  vsnprintf(msg, sizeof(msg), format, args);
  va_end(args);
  snap->cause = snap_err_new_format(snap, err, "%s", msg);
  longjmp(snap->jmp, 1);
}

static SValue parse_expr(Snap* snap, SnapLex* lex, int token);

static SValue parse_sexpr(Snap* snap, SnapLex* lex) {
  SCons* sexpr;
  SCons* tail;
  int token = snap_lex_next_token(lex);
  if (token == ')') {
    return create_empty();
  }
  sexpr = tail = (SCons*)snap_anchor(snap, (SObject*)snap_cons_new(snap));
  sexpr->first = parse_expr(snap, lex, token);
  // parse_expr() raises on EOF, so this always ends at ')'
  while ((token = snap_lex_next_token(lex)) != ')') {
    SCons* next = snap_cons_new(snap);
    tail->rest = create_obj(next);
    tail = next;
    tail->first = parse_expr(snap, lex, token);
  }
  snap_release(snap);
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

static SValue parse_vec(Snap* snap, SnapLex* lex) {
  int token;
  SVec* vec = (SVec*)snap_anchor(snap, (SObject*)snap_vec_new(snap, 0));
  // parse_expr() raises on EOF, so this always ends at ']'
  while ((token = snap_lex_next_token(lex)) != ']') {
    SValue val = parse_expr(snap, lex, token);
    snap_vec_push(snap, vec, val);
  }
  snap_release(snap);
  return create_obj(vec);
}

static SValue parse_map(Snap* snap, SnapLex* lex) {
  int token;
  SMap* map = (SMap*)snap_anchor(snap, (SObject*)snap_map_new(snap));
  // parse_expr() raises on EOF, so this always ends at '}'
  while ((token = snap_lex_next_token(lex)) != '}') {
    SValue key = parse_expr(snap, lex, token), val;
    if (is_obj(key)) snap_anchor(snap, as_obj(key));
    if ((token = snap_lex_next_token(lex)) == '}') {
      raise(snap, "snap_parse_error", "Map literal needs a value for every key on line %d", lex->line);
    }
    val = parse_expr(snap, lex, token);
    snap_map_put(snap, map, key, val);
    if (is_obj(key)) snap_release(snap);
  }
  snap_release(snap);
  return create_obj(map);
}

static SValue parse_quote(Snap* snap, SnapLex* lex) {
  SCons* sexpr = (SCons*)snap_anchor(snap, (SObject*)snap_cons_new(snap));
  sexpr->first = create_form(TK_QUOTE);
  sexpr->rest = create_obj(snap_cons_new(snap));
  as_cons(sexpr->rest)->first = parse_expr(snap, lex, snap_lex_next_token(lex));
  snap_release(snap);
  return create_obj(sexpr);
}

static SValue parse_int(Snap* snap, SnapLex* lex) {
  char* end;
  long long i;
  errno = 0;
  i = strtoll(lex->val, &end, 10);
  if (errno == ERANGE || i < SNAP_INT_MIN || i > SNAP_INT_MAX) {
    raise(snap, "snap_parse_error", "Integer literal is out of range on line %d", lex->line);
  }
  return create_int(i);
}

static SValue parse_expr(Snap* snap, SnapLex* lex, int token) {
  switch (token) {
    case '(':
      return parse_sexpr(snap, lex);
    case '[':
      return parse_vec(snap, lex);
    case '{':
      return parse_map(snap, lex);
    case '\'':
      return parse_quote(snap, lex);
    case TK_INT:
      return parse_int(snap, lex);
    case TK_FLOAT:
      return create_float(snap, atof(lex->val));
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
    case TK_PANIC:
    case TK_TRY:
    case TK_ADD: case TK_SUB: case TK_MUL: case TK_DIV: case TK_MOD:
    case TK_LT: case TK_LE: case TK_GT: case TK_GE: case TK_EQ:
    case TK_NOT: case TK_AND: case TK_OR:
      return create_form(token);
    case TK_EOF:
      raise(snap, "snap_parse_error", "Premature end of file on line %d", lex->line);
      break;
    case TK_TOO_BIG:
      raise(snap, "snap_parse_error", "Literal or identifier is too big on line %d", lex->line);
      break;
    case TK_INVALID:
    default:
      raise(snap, "snap_parse_error", "Invalid token on line %d", lex->line);
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

/* Compiler */

// Compile-time state is plain malloc'd memory, not GC objects. The only GC
// values it holds are constants, which are rooted through snap->fn_state.
// Symbols are interned and live forever, so raw SSym* are safe to keep.

typedef struct SnapLocal_ {
  SSym* name;
  int depth;
  bool captured; // Closed over by an inner fn
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
  struct SnapFnState_* up;
  struct { MVEC_FIELDS(SnapInst); } insts;
  SnapVec constants;
  SnapLocal locals[MAX_LOCALS];
  int num_locals;
  int max_locals;
  uint32_t upvals[MAX_UPVALS]; // (index << 1) | islocal
  int num_upvals;
  int num_args;
  int depth;       // Scope depth; 0 is top-level, where def makes globals
  int stack;       // Operand stack depth at this point in the code
  int max_stack;
  int last_target; // Instruction most recently targeted by a jump
  SnapLoop* loop;  // Innermost recur target, NULL at the top-level
  SnapLoop fn_loop;
} SnapFnState;

static void gc_mark_fn_states(Snap* snap) {
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
  fs->up = snap->fn_state;
  snap->fn_state = fs;
  return fs;
}

static void fn_state_free(Snap* snap) {
  SnapFnState* fs = snap->fn_state;
  snap->fn_state = fs->up;
  mvec_destroy(&fs->insts);
  mvec_destroy(&fs->constants);
  free(fs);
}

static SCode* code_new(Snap* snap, SnapFnState* fs) {
  SCode* c = (SCode*)gc_new(snap, STYPE_CODE, sizeof(SCode));
  c->insts = fs->insts.vitems;
  c->insts_count = fs->insts.vsize;
  c->constants = fs->constants.vitems;
  c->num_constants = fs->constants.vsize;
  c->upvals = (uint32_t*)malloc(fs->num_upvals * sizeof(uint32_t));
  memcpy(c->upvals, fs->upvals, fs->num_upvals * sizeof(uint32_t));
  c->num_upvals = fs->num_upvals;
  c->num_args = fs->num_args;
  c->num_locals = fs->max_locals;
  c->max_stack_size = fs->max_stack + 2; /* Add enough room for raising errors */
  snap->num_bytes_alloced += code_extra_bytes(c);

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

static int emit_binary_operands(SnapFnState* fs, int op, int left, int right) {
  adjust_stack(fs, 1 - (left == 0) - (right == 0)); // Pops stack operands, pushes the result
  *mvec_push(&fs->insts, SnapInst) = INST(op, OPERANDS(0, left, right));
  return pc(fs) - 1;
}

static int emit_binary(SnapFnState* fs, int op) {
  int right = take_operand(fs);
  int left = right ? take_operand(fs) : 0; // Only adjacent if right was a load too
  return emit_binary_operands(fs, op, left, right);
}

static int emit(SnapFnState* fs, int op, int arg) {
  if (is_binary_op(op)) {
    return emit_binary(fs, op);
  }

  // Peepholes, unless a jump lands between the instructions
  if (fs->last_target != pc(fs)) {
    int last = last_op(fs);
    if (op == POP && (last == LOAD_LOCAL || last == LOAD_CLOSED ||
                      last == LOAD_CONSTANT || last == LOAD_NIL)) {
      fs->insts.vsize--;
      adjust_stack(fs, -1);
      return pc(fs) - 1;
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
    case LOAD_GLOBAL: case LOAD_LOCAL: case LOAD_CLOSED:
    case LOAD_CONSTANT: case LOAD_NIL: case DUP:
      adjust_stack(fs, 1);
      break;
    case STORE_GLOBAL: case STORE_LOCAL: case STORE_CLOSED:
    case POP: case RETURN: case PANIC:
    case JUMP_FALSE: case JUMP_FALSE_OR_POP: case JUMP_TRUE_OR_POP:
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
    raise(snap, "snap_compile_error", "Jump is too far");
  }
  fs->insts.vitems[at] = INST(INST_OP(fs->insts.vitems[at]), offset);
  fs->last_target = pc(fs);
}

static void emit_jump_back(Snap* snap, SnapFnState* fs, int target) {
  int offset = target - (pc(fs) + 1);
  if (offset < -MAX_ARG) {
    raise(snap, "snap_compile_error", "Jump is too far");
  }
  emit(fs, JUMP, offset);
}

static int add_constant(Snap* snap, SnapFnState* fs, SValue val) {
  int i;
  for (i = 0; i < fs->constants.vsize; ++i) {
    if (snap_compare(&fs->constants.vitems[i], &val) == 0) return i;
  }
  if (i > MAX_ARG) {
    raise(snap, "snap_compile_error", "Too many constants");
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

static int add_local(Snap* snap, SnapFnState* fs, SSym* name) {
  int i;
  SnapLocal* local;
  for (i = fs->num_locals - 1; i >= 0 && fs->locals[i].depth == fs->depth; --i) {
    if (fs->locals[i].name == name) {
      raise(snap, "snap_compile_error", "Variable '%s' already defined in current scope", name->data);
    }
  }
  if (fs->num_locals >= MAX_LOCALS) {
    raise(snap, "snap_compile_error", "Too many local variables");
  }
  local = &fs->locals[fs->num_locals++];
  local->name = name;
  local->depth = fs->depth;
  local->captured = false;
  if (fs->num_locals > fs->max_locals) {
    fs->max_locals = fs->num_locals;
  }
  return fs->num_locals - 1;
}

static int resolve_local(SnapFnState* fs, SSym* name) {
  int i;
  for (i = fs->num_locals - 1; i >= 0; --i) {
    if (fs->locals[i].name == name) return i;
  }
  return -1;
}

static int add_upval(Snap* snap, SnapFnState* fs, int index, bool islocal) {
  int i;
  uint32_t desc = ((uint32_t)index << 1) | islocal;
  for (i = 0; i < fs->num_upvals; ++i) {
    if (fs->upvals[i] == desc) return i;
  }
  if (fs->num_upvals >= MAX_UPVALS) {
    raise(snap, "snap_compile_error", "Too many closed over variables");
  }
  fs->upvals[fs->num_upvals] = desc;
  return fs->num_upvals++;
}

static int resolve_upval(Snap* snap, SnapFnState* fs, SSym* name) {
  int index;
  if (!fs->up) return -1;
  index = resolve_local(fs->up, name);
  if (index >= 0) {
    fs->up->locals[index].captured = true;
    return add_upval(snap, fs, index, true);
  }
  index = resolve_upval(snap, fs->up, name);
  if (index >= 0) {
    return add_upval(snap, fs, index, false);
  }
  return -1;
}

static void load_variable(Snap* snap, SnapFnState* fs, SSym* name) {
  int index;
  if ((index = resolve_local(fs, name)) >= 0) {
    emit(fs, LOAD_LOCAL, index);
  } else if ((index = resolve_upval(snap, fs, name)) >= 0) {
    emit(fs, LOAD_CLOSED, index);
  } else {
    emit(fs, LOAD_GLOBAL, add_constant(snap, fs, create_obj(name)));
  }
}

static void store_variable(Snap* snap, SnapFnState* fs, SSym* name) {
  int index;
  if ((index = resolve_local(fs, name)) >= 0) {
    emit(fs, STORE_LOCAL, index);
  } else if ((index = resolve_upval(snap, fs, name)) >= 0) {
    emit(fs, STORE_CLOSED, index);
  } else {
    emit(fs, STORE_GLOBAL, add_constant(snap, fs, create_obj(name)));
  }
}

// Conservative check for an inner fn (quoted data can give false positives)
static bool contains_fn(SValue expr) {
  int i;
  if (is_cons(expr)) {
    SCons* curr;
    for (curr = as_cons(expr); curr; curr = as_cons(curr->rest)) {
      if ((is_form(curr->first) && as_form(curr->first) == TK_FN) ||
          contains_fn(curr->first)) {
        return true;
      }
    }
  } else if (is_vec(expr)) {
    for (i = 0; i < as_vec(expr)->len; ++i) {
      if (contains_fn(as_vec(expr)->data[i])) return true;
    }
  } else if (is_map(expr)) {
    for (i = 0; i < 2 * as_map(expr)->used; ++i) {
      if (contains_fn(as_map(expr)->entries[i])) return true;
    }
  }
  return false;
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
      if (references(as_vec(expr)->data[i], name)) return true;
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
    for (i = 0; i < as_vec(expr)->len; ++i) make_read_only(as_vec(expr)->data[i]);
  } else if (is_map(expr)) {
    as_obj(expr)->read_only = true;
    for (i = 0; i < 2 * as_map(expr)->used; ++i) make_read_only(as_map(expr)->entries[i]);
  }
}

static int list_length(SCons* list) {
  int count = 0;
  for (; list; list = as_cons(list->rest)) count++;
  return count;
}

// The nth element; the list must have more than n elements
static SValue nth(SCons* list, int n) {
  while (n-- > 0) list = as_cons(list->rest);
  return list->first;
}

static void compile(Snap* snap, SnapFnState* fs, SValue expr, bool tail);

// A local or literal as a binary op operand, emitting nothing; 0 otherwise
static int direct_operand(Snap* snap, SnapFnState* fs, SValue expr) {
  int index;
  switch (snap_type(expr)) {
    case STYPE_SYM:
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

static void compile_body(Snap* snap, SnapFnState* fs, SCons* body, bool tail) {
  SCons* curr;
  for (curr = body; curr != NULL; curr = as_cons(curr->rest)) {
    if (curr != body) emit(fs, POP, 0);
    compile(snap, fs, curr->first, tail && is_empty(curr->rest));
  }
}

static void compile_def(Snap* snap, SnapFnState* fs, SCons* args) {
  SSym* name;
  if (list_length(args) != 2 || !is_sym(args->first)) {
    raise(snap, "snap_compile_error", "Expected (def <id> <expr>)");
  }
  name = as_sym(args->first);
  compile(snap, fs, nth(args, 1), false);
  if (fs->depth > 0) {
    emit(fs, STORE_LOCAL, add_local(snap, fs, name));
  } else {
    emit(fs, STORE_GLOBAL, add_constant(snap, fs, create_obj(name)));
  }
  emit(fs, LOAD_NIL, 0); /* TODO: Hack */
}

static void compile_set(Snap* snap, SnapFnState* fs, SCons* args) {
  if (list_length(args) != 2 || !is_sym(args->first)) {
    raise(snap, "snap_compile_error", "Expected (set! <id> <expr>)");
  }
  compile(snap, fs, nth(args, 1), false);
  store_variable(snap, fs, as_sym(args->first));
  emit(fs, LOAD_NIL, 0); /* TODO: Hack */
}

static void compile_if(Snap* snap, SnapFnState* fs, SCons* args, bool tail) {
  int cond_jump, jump, stack;
  if (list_length(args) != 3) {
    raise(snap, "snap_compile_error", "Expected (if <cond> <then> <else>)");
  }
  compile(snap, fs, args->first, false);
  cond_jump = emit_jump(fs, JUMP_FALSE);
  stack = fs->stack;

  compile(snap, fs, nth(args, 1), tail);
  jump = emit_jump(fs, JUMP);

  patch_jump(snap, fs, cond_jump);
  fs->stack = stack;
  compile(snap, fs, nth(args, 2), tail);
  patch_jump(snap, fs, jump);
}

static void compile_fn(Snap* snap, SnapFnState* up, SCons* args) {
  int i;
  SVec* params;
  SnapFnState* fs;
  SCode* code;

  if (!args || !is_vec(args->first)) {
    raise(snap, "snap_compile_error", "Expected parameter list");
  }
  if (!as_cons(args->rest)) {
    raise(snap, "snap_compile_error", "Expected expressions");
  }

  params = as_vec(args->first);
  fs = fn_state_new(snap);
  fs->depth = 1; // def inside a fn makes locals

  for (i = 0; i < params->len; ++i) {
    if (!is_sym(params->data[i])) {
      raise(snap, "snap_compile_error", "Expected symbol");
    }
    add_local(snap, fs, as_sym(params->data[i]));
  }
  fs->num_args = params->len;

  fs->fn_loop.top = 0;
  fs->fn_loop.base = 0;
  fs->fn_loop.count = params->len;
  fs->fn_loop.closes = contains_fn(args->rest);
  fs->loop = &fs->fn_loop;

  compile_body(snap, fs, as_cons(args->rest), true);
  emit(fs, RETURN, 0);

  code = code_new(snap, fs);
  fn_state_free(snap);
  load_constant(snap, up, create_obj(code)); // Rooted again as a constant
  if (code->num_upvals > 0) {
    emit(up, CLOSURE, 0);
  }
}

static void compile_let(Snap* snap, SnapFnState* fs, SCons* args, bool loop, bool tail) {
  int i, base;
  SVec* bindings;
  SnapLoop recur_target;

  if (!args || !is_vec(args->first)) {
    raise(snap, "snap_compile_error", "Expected binding vector");
  }
  if (!as_cons(args->rest)) {
    raise(snap, "snap_compile_error", "Expected expressions");
  }

  bindings = as_vec(args->first);
  for (i = 0; i < bindings->len; ++i) {
    SValue binding = bindings->data[i];
    if (!is_vec(binding) || as_vec(binding)->len != 2 || !is_sym(as_vec(binding)->data[0])) {
      raise(snap, "snap_compile_error", "Expected binding pair [<id> <expr>]");
    }
  }

  // Values are evaluated before any of the names are visible
  for (i = 0; i < bindings->len; ++i) {
    compile(snap, fs, as_vec(bindings->data[i])->data[1], false);
  }

  begin_scope(fs);
  base = fs->num_locals;
  for (i = 0; i < bindings->len; ++i) {
    add_local(snap, fs, as_sym(as_vec(bindings->data[i])->data[0]));
  }
  for (i = bindings->len - 1; i >= 0; --i) {
    emit(fs, STORE_LOCAL, base + i);
  }

  if (loop) {
    recur_target.top = pc(fs);
    recur_target.base = base;
    recur_target.count = bindings->len;
    recur_target.closes = contains_fn(args->rest);
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

static void compile_recur(Snap* snap, SnapFnState* fs, SCons* args, bool tail) {
  int i;
  bool stored[MAX_LOCALS];
  SCons* curr;
  SnapLoop* target = fs->loop;

  if (!target) {
    raise(snap, "snap_compile_error", "recur must be inside a fn or loop");
  }
  if (!tail) {
    raise(snap, "snap_compile_error", "recur must be in tail position");
  }
  if (list_length(args) != target->count) {
    raise(snap, "snap_compile_error", "Invalid number of arguments");
  }

  // Each value is stored as soon as it's computed unless a later argument
  // reads the variable, otherwise it's left on the stack and stored after.
  // Captured variables must be closed before they are overwritten, so
  // everything is stored after then.
  for (i = 0, curr = args; curr != NULL; ++i, curr = as_cons(curr->rest)) {
    int slot = target->base + i;
    SCons* later;
    stored[i] = is_same_local(fs, curr->first, slot);
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

  emit_jump_back(snap, fs, target->top);
}

static void compile_panic(Snap* snap, SnapFnState* fs, SCons* args) {
  if (list_length(args) != 2) {
    raise(snap, "snap_compile_error", "Expected (panic <err> <msg>)");
  }
  compile(snap, fs, nth(args, 1), false);
  compile(snap, fs, args->first, false);
  emit(fs, PANIC, 0);
}

bool builtin_vector(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_map(Snap* snap, const SValue* args, int num_args, SValue* result);

// Vectors are mutable, so a literal makes a new one each time it's evaluated
static void compile_vec(Snap* snap, SnapFnState* fs, SVec* vec) {
  int i;
  for (i = 0; i < vec->len; ++i) {
    compile(snap, fs, vec->data[i], false);
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

static void compile_try(Snap* snap, SnapFnState* fs, SCons* args) {
  if (list_length(args) != 1) {
    raise(snap, "snap_compile_error", "Expected (try <expr>)");
  }
  compile(snap, fs, args->first, false);
  emit(fs, TRY, 0);
}

static void compile_arith(Snap* snap, SnapFnState* fs, int opcode, SCons* args) {
  SCons* curr;
  if (list_length(args) < 2) {
    raise(snap, "snap_compile_error", "Expected expressions");
  }
  compile_binary(snap, fs, opcode, args->first, nth(args, 1));
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
    raise(snap, "snap_compile_error", "Expected expressions");
  }
  if (count == 2) {
    compile_binary(snap, fs, opcode, args->first, nth(args, 1));
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

static void compile_sexpr(Snap* snap, SnapFnState* fs, SCons* sexpr, bool tail) {
  SCons* args = as_cons(sexpr->rest);
  switch (snap_type(sexpr->first)) {
    case STYPE_SYM:
    case STYPE_CONS:
      compile_call(snap, fs, sexpr);
      break;
    case STYPE_FORM:
      switch (as_form(sexpr->first)) {
        case TK_BEGIN:
          if (!args) {
            raise(snap, "snap_compile_error", "Expected expressions");
          }
          compile_body(snap, fs, args, tail);
          break;
        case TK_DEF:
          compile_def(snap, fs, args);
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
            raise(snap, "snap_compile_error", "Expected a single expression");
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
        case TK_ADD: case TK_SUB: case TK_MUL: case TK_DIV: case TK_MOD:
          compile_arith(snap, fs, (as_form(sexpr->first) - TK_ADD) + ADD, args);
          break;
        case TK_LT: case TK_LE: case TK_GT: case TK_GE: case TK_EQ:
          compile_compare(snap, fs, (as_form(sexpr->first) - TK_LT) + LESS_THAN, args);
          break;
        case TK_NOT:
          if (list_length(args) != 1) {
            raise(snap, "snap_compile_error", "Expected expression");
          }
          compile(snap, fs, args->first, false);
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
          raise(snap, "snap_compile_error", "Unexpected special form");
          break;
      }
      break;
    default:
      raise(snap, "snap_compile_error", "Expected id, special form or expr");
      break;
  }
}

// `tail` is whether expr is in tail position relative to its recur target
static void compile(Snap* snap, SnapFnState* fs, SValue expr, bool tail) {
  int stack = fs->stack;
  switch (snap_type(expr)) {
    case STYPE_INT:
    case STYPE_FLOAT:
    case STYPE_STR:
    case STYPE_BOOL:
    case STYPE_KEY:
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
    default:
      raise(snap, "snap_compile_error", "Unexpected type");
      break;
  }
  // Every expression leaves exactly one value (even when it ends in a jump,
  // like recur, or never finishes, like panic)
  fs->stack = stack;
  adjust_stack(fs, 1);
}

SValue snap_exec(Snap* snap, const char* str) {
  SnapLex lex;
  SValue expr, result;
  SnapFnState* fs;
  SCode* code;
  bool first = true;
  int num_anchors = snap->anchors.vsize;

  lex.buf = str;
  lex.buf_size = strlen(lex.buf);
  lex.p = lex.buf;
  lex.line = 1;

  if (setjmp(snap->jmp) > 0) {
    while (snap->fn_state) fn_state_free(snap);
    result = create_obj(snap->cause);
    goto done;
  }

  fs = fn_state_new(snap);
  while (parse(snap, &lex, &expr)) {
    if (is_obj(expr)) snap_anchor(snap, as_obj(expr));
#ifdef SNAP_DEBUG
    snap_print(expr);
    printf("\n");
#endif
    if (!first) emit(fs, POP, 0);
    first = false;
    compile(snap, fs, expr, false);
    if (is_obj(expr)) snap_release(snap);
  }
  if (first) emit(fs, LOAD_NIL, 0);
  emit(fs, RETURN, 0);

  code = (SCode*)snap_anchor(snap, (SObject*)code_new(snap, fs));
  fn_state_free(snap);
  if (snap->dump_code) print_code(code, 0);
  push_frame(snap, code, snap->stack, NULL);
  result = exec(snap);

done:
  snap->anchors.vsize = num_anchors; // Also unwinds anchors left by a longjmp
  return result;
}

/* Printing */

void snap_print(SValue val) {
  print_val(val);
}

static void iprintf(int indent, const char* format, ...) {
  va_list args;
  va_start(args, format);
  printf("%*s", indent, "");
  vprintf(format, args);
  va_end(args);
}

static const char* form_name(int token) {
  switch (token) {
    case TK_BEGIN: return "begin";
    case TK_DEF: return "def";
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
    case TK_LT: return "<";
    case TK_LE: return "<=";
    case TK_GT: return ">";
    case TK_GE: return ">=";
    case TK_EQ: return "=";
    case TK_NOT: return "not";
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

static void print_val(SValue val) {
  int i;
  SCons* cons;
  switch (snap_type(val)) {
    case STYPE_NIL:
      printf("nil");
      break;
    case STYPE_BOOL:
      printf("%s", is_false(val) ? "false" : "true");
      break;
    case STYPE_INT:
      printf("%ld", (long)as_int(val));
      break;
    case STYPE_FLOAT:
      printf("%f", as_float(val));
      break;
    case STYPE_CFUNC:
      printf("<cfunc %p>", (void*)as_cfunc(val));
      break;
    case STYPE_FORM:
      printf("%s", form_name(as_form(val)));
      break;
    case STYPE_STR:
      printf("\"%s\"", as_str(val)->data);
      break;
    case STYPE_SYM:
      printf("%s", as_sym(val)->data);
      break;
    case STYPE_KEY:
      printf(":%s", as_key(val)->data);
      break;
    case STYPE_ERR:
      printf("<err ");
      print_val(as_err(val)->err);
      printf(" ");
      print_val(as_err(val)->msg);
      printf(">");
      break;
    case STYPE_VEC:
      printf("[");
      for (i = 0; i < as_vec(val)->len; ++i) {
        if (i > 0) printf(" ");
        print_val(as_vec(val)->data[i]);
      }
      printf("]");
      break;
    case STYPE_MAP: {
      SMap* m = as_map(val);
      bool first = true;
      printf("{");
      for (i = 0; i < m->used; ++i) {
        if (m->entries[2 * i].bits == SV_DELETED) continue;
        if (!first) printf(" ");
        first = false;
        print_val(m->entries[2 * i]);
        printf(" ");
        print_val(m->entries[2 * i + 1]);
      }
      printf("}");
      break;
    }
    case STYPE_CONS:
      printf("(");
      for (cons = as_cons(val); cons != NULL; ) {
        print_val(cons->first);
        if (!is_cons(cons->rest)) {
          printf(" . ");
          print_val(cons->rest);
          break;
        }
        cons = as_cons(cons->rest);
        if (cons) printf(" ");
      }
      printf(")");
      break;
    case STYPE_CODE:
      printf("<fn %p>", (void*)as_obj(val));
      break;
    case STYPE_CLOSURE:
      printf("<closure %p>", (void*)as_obj(val));
      break;
  }
}

static void print_code(SCode* code, int indent) {
  int i;
  iprintf(indent, "args: %d, locals: %d, max stack: %d\n",
          code->num_args, code->num_locals, code->max_stack_size);
  for (i = 0; i < code->num_upvals; ++i) {
    iprintf(indent, "upval %d: %s %d\n", i,
            (code->upvals[i] & 1) ? "local" : "upval", code->upvals[i] >> 1);
  }
  for (i = 0; i < code->insts_count; ++i) {
    int op = INST_OP(code->insts[i]);
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
    switch (op) {
      case LOAD_GLOBAL: case STORE_GLOBAL: case LOAD_CONSTANT:
        printf(" %-6d ", arg);
        print_val(code->constants[arg]);
        break;
      case JUMP: case JUMP_FALSE: case JUMP_FALSE_OR_POP: case JUMP_TRUE_OR_POP:
        printf(" %-6d (to %d)", arg, i + 1 + arg);
        break;
      case LOAD_LOCAL: case STORE_LOCAL: case LOAD_CLOSED: case STORE_CLOSED:
      case CALL: case CLOSE_UPVALS:
        printf(" %d", arg);
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

/* Builtins */

// Sets *result to a new error and returns false, so a builtin can panic with
// `return fail(...)`
static bool fail(Snap* snap, SValue* result, const char* err, const char* msg) {
  *result = create_obj(snap_err_new_format(snap, err, "%s", msg));
  return false;
}

static bool arity_error(Snap* snap, SValue* result) {
  return fail(snap, result, "snap_arity_error", "Invalid number of arguments");
}

static bool read_only_error(Snap* snap, SValue* result) {
  return fail(snap, result, "snap_type_error", "Can't change a quoted literal");
}

bool builtin_print(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int i;
  for (i = 0; i < num_args; ++i) {
    if (i > 0) printf(" ");
    snap_print(args[i]);
  }
  return true;
}

bool builtin_println(Snap* snap, const SValue* args, int num_args, SValue* result) {
  builtin_print(snap, args, num_args, result);
  printf("\n");
  return true;
}

bool builtin_isnil(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_bool(is_nil(args[0]));
  return true;
}

bool builtin_isvec(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_bool(is_vec(args[0]));
  return true;
}

bool builtin_ismap(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_bool(is_map(args[0]));
  return true;
}

// (err <kind> <msg>) makes an error value. Unlike panic it doesn't unwind;
// it's returned like any other value (see try).
bool builtin_err(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 2) return arity_error(snap, result);
  *result = create_obj(snap_err_new(snap, args[0], args[1]));
  return true;
}

// (err? <x>) or (err? <x> <kind>)
bool builtin_iserr(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1 && num_args != 2) return arity_error(snap, result);
  *result = create_bool(is_err(args[0]) &&
                        (num_args == 1 || snap_compare(&as_err(args[0])->err, &args[1]) == 0));
  return true;
}

static bool err_arg(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  if (!is_err(args[0])) return fail(snap, result, "snap_type_error", "Expected an error");
  return true;
}

bool builtin_err_kind(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (!err_arg(snap, args, num_args, result)) return false;
  *result = as_err(args[0])->err;
  return true;
}

bool builtin_err_msg(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (!err_arg(snap, args, num_args, result)) return false;
  *result = as_err(args[0])->msg;
  return true;
}

// Builds vector literals (see compile_vec())
bool builtin_vector(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SVec* vec = snap_vec_new(snap, num_args);
  if (num_args > 0) memcpy(vec->data, args, num_args * sizeof(SValue));
  vec->len = num_args;
  *result = create_obj(vec);
  return true;
}

// Checks for a vector, and an index that's in range when there is one
static bool vec_args(Snap* snap, const SValue* args, int num_args, int expected, SValue* result) {
  if (num_args != expected) return arity_error(snap, result);
  if (!is_vec(args[0])) return fail(snap, result, "snap_type_error", "Expected a vector");
  if (expected > 1) {
    if (!is_int(args[1])) return fail(snap, result, "snap_type_error", "Expected an integer index");
    if (as_int(args[1]) < 0 || as_int(args[1]) >= as_vec(args[0])->len) {
      return fail(snap, result, "snap_index_error", "Index out of range");
    }
  }
  return true;
}

// Builds map literals (see compile_map()) from key/value pairs
bool builtin_map(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int i;
  SMap* map = snap_map_new(snap);
  for (i = 0; i < num_args; i += 2) {
    snap_map_put(snap, map, args[i], args[i + 1]);
  }
  *result = create_obj(map);
  return true;
}

bool builtin_count(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  if (is_vec(args[0])) {
    *result = create_int(as_vec(args[0])->len);
  } else if (is_map(args[0])) {
    *result = create_int(as_map(args[0])->len);
  } else {
    return fail(snap, result, "snap_type_error", "Expected a vector or map");
  }
  return true;
}

// (nth <vec> <index>)
bool builtin_nth(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (!vec_args(snap, args, num_args, 2, result)) return false;
  *result = as_vec(args[0])->data[as_int(args[1])];
  return true;
}

// (put! <vec> <index> <val>) or (put! <map> <key> <val>) returns the vector or map
bool builtin_put(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args == 3 && is_map(args[0])) {
    if (as_obj(args[0])->read_only) return read_only_error(snap, result);
    snap_map_put(snap, as_map(args[0]), args[1], args[2]);
    *result = args[0];
    return true;
  }
  if (!vec_args(snap, args, num_args, 3, result)) return false;
  if (as_obj(args[0])->read_only) return read_only_error(snap, result);
  as_vec(args[0])->data[as_int(args[1])] = args[2];
  *result = args[0];
  return true;
}

// (push! <vec> <val>...) appends to the end and returns the vector
bool builtin_push(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int i;
  if (num_args < 2) return arity_error(snap, result);
  if (!is_vec(args[0])) return fail(snap, result, "snap_type_error", "Expected a vector");
  if (as_obj(args[0])->read_only) return read_only_error(snap, result);
  for (i = 1; i < num_args; ++i) {
    snap_vec_push(snap, as_vec(args[0]), args[i]);
  }
  *result = args[0];
  return true;
}

// (pop! <vec>) removes and returns the last value
bool builtin_pop(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SVec* vec;
  if (!vec_args(snap, args, num_args, 1, result)) return false;
  vec = as_vec(args[0]);
  if (vec->read_only) return read_only_error(snap, result);
  if (vec->len == 0) return fail(snap, result, "snap_index_error", "Vector is empty");
  *result = vec->data[--vec->len];
  return true;
}

// (get <map> <key>) or (get <map> <key> <default>). The default is nil.
bool builtin_get(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SValue* val;
  if (num_args != 2 && num_args != 3) return arity_error(snap, result);
  if (!is_map(args[0])) return fail(snap, result, "snap_type_error", "Expected a map");
  val = snap_map_get(as_map(args[0]), args[1]);
  *result = val ? *val : (num_args == 3 ? args[2] : create_nil());
  return true;
}

// (del! <map> <key>) returns the map, whether or not the key was there
bool builtin_del(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 2) return arity_error(snap, result);
  if (!is_map(args[0])) return fail(snap, result, "snap_type_error", "Expected a map");
  if (as_obj(args[0])->read_only) return read_only_error(snap, result);
  snap_map_del(as_map(args[0]), args[1]);
  *result = args[0];
  return true;
}

// The keys (offset 0) or values (offset 1) as a vector, in insertion order
static bool map_entries(Snap* snap, const SValue* args, int num_args, int offset, SValue* result) {
  SMap* map;
  SVec* vec;
  int i;
  if (num_args != 1) return arity_error(snap, result);
  if (!is_map(args[0])) return fail(snap, result, "snap_type_error", "Expected a map");
  map = as_map(args[0]);
  vec = snap_vec_new(snap, map->len);
  for (i = 0; i < map->used; ++i) {
    if (map->entries[2 * i].bits != SV_DELETED) vec->data[vec->len++] = map->entries[2 * i + offset];
  }
  *result = create_obj(vec);
  return true;
}

bool builtin_keys(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return map_entries(snap, args, num_args, 0, result);
}

bool builtin_vals(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return map_entries(snap, args, num_args, 1, result);
}

void snap_init(Snap* snap) {
  snap->stack_size = 64;
  snap->stack = (SValue*)malloc(snap->stack_size * sizeof(SValue));
  snap->stack_end = snap->stack + snap->stack_size;
  mvec_init(&snap->frames, SnapFrame, 32);
  mvec_init(&snap->anchors, SObject*, 32);
  snap->num_bytes_alloced = 0;
  snap->next_gc = GC_MIN_STEP;
  snap->all = NULL;
  mvec_init(&snap->gray, SObject*, 64);
  snap->open_upvals = NULL;
  snap->fn_state = NULL;
  snap->cause = NULL;
  snap->dump_code = false;
  snap_hash_init(&snap->symbols);
  snap_hash_init(&snap->keywords);

  snap_def_cfunc(snap, "print", builtin_print);
  snap_def_cfunc(snap, "println", builtin_println);
  snap_def_cfunc(snap, "nil?", builtin_isnil);
  snap_def_cfunc(snap, "vector?", builtin_isvec);
  snap_def_cfunc(snap, "map?", builtin_ismap);
  snap_def_cfunc(snap, "err", builtin_err);
  snap_def_cfunc(snap, "err?", builtin_iserr);
  snap_def_cfunc(snap, "err-kind", builtin_err_kind);
  snap_def_cfunc(snap, "err-msg", builtin_err_msg);
  snap_def_cfunc(snap, "count", builtin_count);
  snap_def_cfunc(snap, "nth", builtin_nth);
  snap_def_cfunc(snap, "put!", builtin_put);
  snap_def_cfunc(snap, "push!", builtin_push);
  snap_def_cfunc(snap, "pop!", builtin_pop);
  snap_def_cfunc(snap, "get", builtin_get);
  snap_def_cfunc(snap, "del!", builtin_del);
  snap_def_cfunc(snap, "keys", builtin_keys);
  snap_def_cfunc(snap, "vals", builtin_vals);
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
  mvec_destroy(&snap->gray);
  snap_hash_destroy(&snap->symbols);
  snap_hash_destroy(&snap->keywords);
  assert(snap->num_bytes_alloced == 0);
}
