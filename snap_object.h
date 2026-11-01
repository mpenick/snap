// Values and objects (snap_object.c): hashing, constructors, maps, vectors,
// lists, structs and printing. The ones the VM uses on every instruction
// are inlined.
#ifndef SNAP_SNAP_OBJECT_H
#define SNAP_SNAP_OBJECT_H

#include "snap_gc.h"
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#define map_bytes(cap) ((size_t)(cap) * 2 * (sizeof(SValue) + sizeof(int32_t)))

// Where a small vector keeps its values (see snap_vec_new())
#define vec_inline(v) ((SValue*)((SVec*)(v) + 1))

// Appends a cell to a list being built and returns the slot for its value.
// The list is anchored from its first cell until list_done(), so the caller
// can allocate (even to make the value) before filling the slot.
typedef struct {
  SCons* head;
  SCons* tail;
} SnapList;

void snap_hash_init(SnapHash* hash);
void snap_hash_put(SnapHash* hash, SValue key, SValue val);
SValue* snap_hash_get(SnapHash* hash, SValue key);
SValue* snap_hash_get_str(SnapHash* hash, const char* key, size_t len);
void snap_hash_destroy(SnapHash* hash);
SStr* snap_str_new_vformat(Snap* snap, const char* format, va_list args);
SStr* snap_str_new_format(Snap* snap, const char* format, ...);
SKeyword* snap_key_new(Snap* snap, const char* name);
SClosure* snap_closure_new(Snap* snap, SCode* code);
void map_rebuild(Snap* snap, SMap* map);
SErr* method_error(Snap* snap, SMethod* m, const SValue* args, int num_args);
void print_val(FILE* out, SValue val);
bool struct_new(Snap* snap, SType* type, const SValue* args, int num_args, SValue* result);
SType* type_new(Snap* snap, const char* name, SVec* fields);

// Returns the key's entry, or -1. Linear probing; deleted entries keep their
// index slots so that probing continues past them.
static inline int map_find(SMap* map, SValue key) {
  int i, mask = 2 * map->cap - 1;
  if (map->cap == 0) return -1;
  for (i = snap_hash(&key) & mask; map->index[i] >= 0; i = (i + 1) & mask) {
    SValue* k = &map->entries[2 * map->index[i]];
    if (k->bits == key.bits || snap_compare(k, &key) == 0) return map->index[i];
  }
  return -1;
}

static inline SValue* map_get(SMap* map, SValue key) {
  int e = map_find(map, key);
  return e < 0 ? NULL : &map->entries[2 * e + 1];
}

// The slot of a struct's field, or NULL if it doesn't have it
static inline int field_index(SType* type, SValue key) {
  int i;
  for (i = 0; i < type->num_fields; ++i) {
    if (type->fields[i].bits == key.bits) return i;
  }
  return -1;
}

static inline SValue* struct_field(SStruct* st, SValue key) {
  int i = field_index(st->stype, key);
  return i < 0 ? NULL : &st->vals[i];
}

// The index of field key in st, or -1, with the call site's cache (slot, 0
// for none): a hit is the struct's type and key it saw last
static inline int cached_field(SCode* code, int slot, SStruct* st, SValue key) {
  SFieldCache* fc;
  int i;
  if (slot == 0) return field_index(st->stype, key);
  fc = &code->caches[slot - 1];
  if (fc->type == st->stype && fc->key.bits == key.bits) return fc->index;
  i = field_index(st->stype, key);
  if (i >= 0) { // Only field keywords, which are interned, are kept
    fc->type = st->stype;
    fc->key = key;
    fc->index = i;
  }
  return i;
}

static inline SType* type_of(Snap* snap, SValue val) {
  return is_struct(val) ? as_struct(val)->stype : snap->types[snap_type(val)];
}

// The function a method call runs, picked by its first argument's type, or
// NULL if it doesn't have one
static inline SValue* method_find(Snap* snap, SMethod* m, const SValue* args, int num_args) {
  return num_args > 0 ? map_get(m->table, create_obj(type_of(snap, args[0]))) : NULL;
}

// The i-th live entry (its key, then its value), in insertion order, or NULL
// if there isn't one. After a deletion it first closes the gaps (which keeps
// the order), so walking a map by index stays constant time per entry.
static inline SValue* map_entry_at(Snap* snap, SMap* map, SnapInt i) {
  if (i < 0 || i >= map->len) return NULL;
  if (map->used != map->len) map_rebuild(snap, map);
  return &map->entries[2 * i];
}

static inline SValue float_new(Snap* snap, SnapFloat f) {
  SValue val;
  SFloat* n;
  if (snap_imm_float(f, &val)) return val;
  n = (SFloat*)gc_new(snap, STYPE_FLOAT, sizeof(SFloat));
  n->f = f;
  return create_obj(n);
}

static inline SClosure* closure_new(Snap* snap, SCode* code) {
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

static inline SCons* cons_new(Snap* snap) {
  SCons* c = (SCons*)gc_new(snap,  STYPE_CONS, sizeof(SCons));
  c->first = create_undef();
  c->rest = create_empty();
  return c;
}

static inline SValue* list_add(Snap* snap, SnapList* list) {
  SCons* cell = cons_new(snap);
  if (list->head) {
    list->tail->rest = create_obj(cell);
  } else {
    list->head = (SCons*)anchor(snap, (SObject*)cell);
  }
  list->tail = cell;
  return &cell->first;
}

static inline SValue list_done(Snap* snap, SnapList* list) {
  if (!list->head) return create_empty();
  release(snap);
  return create_obj(list->head);
}

static inline int list_length(SCons* list) {
  int count = 0;
  for (; list; list = as_cons(list->rest)) count++;
  return count;
}

// The nth element; the list must have more than n elements
static inline SValue list_nth(SCons* list, int n) {
  while (n-- > 0) list = as_cons(list->rest);
  return list->first;
}

// Adds a segment when it's full: it never copies values, and the segment
// pointers it copies are at most about 30. Doesn't allocate GC objects, so it
// never collects.
static inline void vec_push(Snap* snap, SVec* vec, SValue val) {
  if (vec->len == snap_vec_cap(vec)) {
    int k = vec->num_segs++;
    size_t size = ((size_t)SNAP_VEC_SEG << k) * sizeof(SValue);
    vec->segs = (SValue**)realloc(vec->segs, vec->num_segs * sizeof(SValue*));
    vec->segs[k] = (SValue*)malloc(size);
    heap_add(snap, size);
  }
  *snap_vec_at(vec, vec->len++) = val;
}

// Frees the last segment once the one before it is empty too, so that pushing
// and popping across a segment boundary doesn't allocate every time
static inline void vec_shrink(Snap* snap, SVec* vec) {
  int n = vec->num_segs;
  if (n >= 2 && vec->len <= vec->cap0 + (SNAP_VEC_SEG << (n - 2)) - SNAP_VEC_SEG) {
    free(vec->segs[n - 1]);
    vec->num_segs--;
    snap->num_bytes_alloced -= ((size_t)SNAP_VEC_SEG << (n - 1)) * sizeof(SValue);
  }
}

// A new vector of args, for (vector ...) and vector literals
static inline bool vec_from_args(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SVec* vec = snap_vec_new(snap, num_args);
  if (num_args > 0) memcpy(vec->data, args, num_args * sizeof(SValue));
  vec->len = num_args;
  *result = create_obj(vec);
  return true;
}

#endif // SNAP_SNAP_OBJECT_H
