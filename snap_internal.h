// Internals shared by the core's files. Embedders include only snap.h.
//
//   snap.c           values, hashing, objects, printing, snap_init() and the API
//   snap_gc.c        the collector, finalizers, pins and the allocation profile
//   snap_vm.c        frames, upvals and the bytecode interpreter
//   snap_parse.c     the reader (the only file that includes snap_lex.h)
//   snap_compile.c   the compiler and the disassembler
//   snap_builtins.c  the built-in functions
//
// Helpers the interpreter needs inlined are static inline at the end, so
// splitting the core into files doesn't cost a call where one file used to
// inline them.
#ifndef SNAP_INTERNAL_H
#define SNAP_INTERNAL_H

#include "snap.h"

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// The collector is incremental mark and sweep. A collection starts once the
// heap has grown to GC_HEAP_GROWTH times what the last one found live, and by
// at least GC_MIN_STEP bytes. It then runs in steps: every GC_STEP_SIZE bytes
// allocated does GC_STEP_MUL units of work (one is marking a value or
// sweeping an object) per 8 bytes, so it keeps ahead of the program. A step
// marks at most GC_CHUNK values of one object. Sweeping counts each free() an
// object needs as GC_FREE_COST more units, so that a step freeing many
// vectors takes about as long as any other.
//
// Units still cost different amounts (a value pointing at an object that's
// not in the cache costs more than a number), so the steps that allocation
// makes also stop after GC_STEP_BUDGET nanoseconds, looking at the clock every
// GC_CLOCK_UNITS units. The work left over is carried into the next step,
// unless more than GC_MAX_CARRY units would be, so the collector can't fall
// behind. 0 turns the budget off. (gc-step n) always does n units.
#ifndef GC_HEAP_GROWTH
#define GC_HEAP_GROWTH 2
#endif

#ifndef GC_MIN_STEP
#define GC_MIN_STEP (1024 * 1024)
#endif

#ifndef GC_STEP_SIZE
#define GC_STEP_SIZE 4096
#endif

#ifndef GC_STEP_MUL
#define GC_STEP_MUL 4
#endif

#define GC_CHUNK 64
#define GC_FREE_COST 8

#ifndef GC_STEP_BUDGET
#define GC_STEP_BUDGET 15000
#endif

#define GC_CLOCK_UNITS 256
#define GC_MAX_CARRY (8 * GC_STEP_SIZE * GC_STEP_MUL / sizeof(SValue))

// Objects up to GC_MAX_SLOT bytes are allocated from free lists of slots of
// a few sizes, carved from GC_PAGE_SIZE pages, so allocating and freeing take
// constant time. Bigger ones get a page of their own. Pages of slots aren't
// given back, so a program that stays the same size stops calling malloc().
#define GC_PAGE_SIZE 16384
#define GC_MAX_SLOT 256

// With GC_POOL 0 every object gets its own page, freed with free(), so that
// ASan sees each use after free (see make stress)
#ifndef GC_POOL
#define GC_POOL 1
#endif
#define SLOT_FREE 0 // Type of a free slot (no object is STYPE_UNDEF)

#define page_slot(page, i) ((SObject*)((char*)((page) + 1) + (size_t)(i) * (page)->slot_size))

#define MAX_ARG ((1 << 23) - 1)
#define MAX_LOCALS 256
#define MAX_UPVALS 256

enum {
  GC_IDLE,
  GC_MARK,
  GC_SWEEP
};

// Mark value of objects that are never freed (symbols, keywords and types). Others
// are 0 or 1, see gc_unmarked.
#define GC_FIXED 2

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
  XX(CALL_GLOBAL) \
  XX(NTH) \
  XX(COUNT) \
  XX(GET) \
  XX(PUT) \
  XX(INT) \
  XX(MIN) \
  XX(MAX) \
  XX(CLOSURE) \
  XX(CLOSE_UPVALS) \
  XX(RETURN) \
  XX(RETURN_VEC) \
  XX(UNPACK) \
  XX(JUMP) \
  XX(JUMP_FALSE) \
  XX(JUMP_TRUE) \
  XX(JUMP_FALSE_OR_POP) \
  XX(JUMP_TRUE_OR_POP) \
  XX(TRY) \
  XX(RECOVER) \
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
  XX(NOT) \
  XX(STEP_LT) \
  XX(STEP_LE) \
  XX(STEP_GT) \
  XX(STEP_GE) \
  BINARY_VARIANTS(XX, ADD) \
  BINARY_VARIANTS(XX, SUB) \
  BINARY_VARIANTS(XX, MUL) \
  BINARY_VARIANTS(XX, DIV) \
  BINARY_VARIANTS(XX, MOD) \
  BINARY_VARIANTS(XX, LESS_THAN) \
  BINARY_VARIANTS(XX, LESS_EQUALS) \
  BINARY_VARIANTS(XX, GREATER_THAN) \
  BINARY_VARIANTS(XX, GREATER_EQUALS) \
  BINARY_VARIANTS(XX, EQUALS)

// Each binary op has a variant for every kind of left and right operand:
// S (stack), L (local) or K (constant), in this order. They have the same
// encoding; specialize() picks them once a function is compiled.
#define BINARY_VARIANTS(XX, op) \
  XX(op##_SS) XX(op##_SL) XX(op##_SK) \
  XX(op##_LS) XX(op##_LL) XX(op##_LK) \
  XX(op##_KS) XX(op##_KL) XX(op##_KK)

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

#define operand_kind(n) ((n) == 0 ? 0 : (n) > 0 ? 1 : 2) // S, L or K
#define variant(op, inst) \
  (ADD_SS + ((op) - ADD) * 9 + operand_kind(OPERAND_LEFT(inst)) * 3 + operand_kind(OPERAND_RIGHT(inst)))
#define variant_base(op) ((op) >= ADD_SS ? ADD + ((op) - ADD_SS) / 9 : (op))

/* Lexer */

// Above 127, since characters like '(' are their own tokens
enum {
  TK_INVALID = 128,
  TK_TOO_BIG,
  TK_INT,
  TK_FLOAT,
  TK_STR,
  TK_ID,
  TK_KEY,
  TK_TRUE,
  TK_FALSE,
  TK_NIL,
  TK_BEGIN,
  TK_DEF,
  TK_DEFMACRO,
  TK_ELLIPSIS,
  TK_IF,
  TK_FN,
  TK_LET,
  TK_LOOP,
  TK_QUOTE,
  TK_RECUR,
  TK_SET,
  TK_PANIC,
  TK_TRY,
  TK_RECOVER,
  TK_ADD, TK_SUB, TK_MUL, TK_DIV, TK_MOD,
  TK_LT, TK_LE, TK_GT, TK_GE, TK_EQ,
  TK_AND, TK_OR, TK_NOT,
  TK_UNQUOTE, // ~x, never lexed
  TK_SPLICE,  // ~@x
  TK_EOF
};

typedef struct {
  const char* buf;
  size_t buf_size;
  const char* p;
  const char* mark;
  int line;
  char val[128];
} SnapLex;

int snap_lex_next_token(SnapLex* lex);

/* Shared macros and types */

// Index into slot_sizes for an object of n bytes, by (n + 15) / 16
static const uint8_t slot_size_index[GC_MAX_SLOT / 16 + 1] = {
  0, 0, 1, 2, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7
};

typedef struct {
  SOBJECT_FIELDS
  SObject* next;
} SFreeSlot;

// One row of the profile: what the code (NULL outside snap code, FREED_CODE
// once its function was freed) made of one kind of object
typedef struct SnapAllocSite_ {
  SCode* code;
  uint8_t type;
  uint64_t count;
  uint64_t bytes;
} SnapAllocSite;

#define FREED_CODE ((SCode*)1)

#define map_bytes(cap) ((size_t)(cap) * 2 * (sizeof(SValue) + sizeof(int32_t)))

// Where a small vector keeps its values (see snap_vec_new())
#define vec_inline(v) ((SValue*)((SVec*)(v) + 1))

// Snapshot-at-the-beginning write barrier: while marking, a heap value about
// to be overwritten or removed is marked first. So everything reachable when
// the collection started gets marked, however the program changes the heap.
// Stores to the stack don't need it (it's marked when the collection starts),
// and neither do stores that only add values.
#define gc_barrier(snap, old) \
  do { if ((snap)->gc_phase == GC_MARK && is_obj(old)) gc_mark(snap, as_obj(old)); } while (0)

#define is_int_p(v) is_int(*(v))
#define is_num_p(v) (is_int_p(v) || is_float(*(v)))
#define num_p(v) (is_int_p(v) ? (SnapFloat)as_int(*(v)) : as_float(*(v)))

// Whether a float truncates to a 63-bit int (false for NaN)
#define in_int_range(f) ((f) >= (SnapFloat)SNAP_INT_MIN && (f) < -(SnapFloat)SNAP_INT_MIN)

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

// apply's last argument: a vector, a list or nil
#define is_seq(v) (is_vec(v) || is_cons(v) || is_nil(v))

// Appends a cell to a list being built and returns the slot for its value.
// The list is anchored from its first cell until list_done(), so the caller
// can allocate (even to make the value) before filling the slot.
typedef struct {
  SCons* head;
  SCons* tail;
} SnapList;

/* Functions shared between files */

// snap.c
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

// snap_gc.c
size_t code_extra_bytes(SCode* code);
int gc_free(Snap* snap, SObject* obj);
__attribute__((cold, noinline)) void profile_alloc(Snap* snap, uint8_t type, size_t size);
void gc_add_page(Snap* snap, SnapPage* page);
void gc_new_page(Snap* snap, int c);
bool gc_pause(Snap* snap, bool start, size_t work, size_t* left);

// snap_parse.c
void parse_enter(Snap* snap, const char* err);
bool parse_form(Snap* snap, SnapLex* lex, SValue* result);
void parse_raise(Snap* snap, const char* err, const char* format, ...);

// snap_compile.c
SValue compile_forms(Snap* snap, SnapLex* lex, bool all);
const char* form_name(int token);
void gc_mark_fn_states(Snap* snap);
SValue macro_expand(Snap* snap, SValue macro, SCons* args);

// snap_builtins.c
void snap_init_builtins(Snap* snap);
bool op_value(int token, SValue* val);
bool builtin_apply(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_concat(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_count(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_gensym(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_get(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_int(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_list(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_make_macro(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_map(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_max(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_min(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_nth(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_nthnext(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_put(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_vec(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_vector(Snap* snap, const SValue* args, int num_args, SValue* result);

/* Inlined helpers */

// Symbols, keywords and types are GC_FIXED, so they never get past the first
// line
static inline void gc_mark(Snap* snap, SObject* obj) {
  if (!obj || obj->mark != snap->gc_unmarked) return;
  obj->mark = snap->gc_unmarked ^ 1;
  switch (obj->type) {
    case STYPE_FLOAT:
    case STYPE_STR:
      break; // No children
    case STYPE_ERR:
    case STYPE_CONS:
    case STYPE_VEC:
    case STYPE_MAP:
    case STYPE_CODE:
    case STYPE_CLOSURE:
    case STYPE_UPVAL:
    case STYPE_STRUCT:
    case STYPE_METHOD: {
      SnapGray* gray = mvec_push(&snap->gray, SnapGray);
      gray->obj = obj;
      gray->pos = 0;
      break;
    }
    default:
      assert(0 && "Not a GC object");
      break;
  }
}

static inline void gc_mark_val(Snap* snap, SValue val) {
  if (is_obj(val)) gc_mark(snap, as_obj(val));
}

static inline void gc_mark_vals(Snap* snap, SValue* vals, int count) {
  int i;
  for (i = 0; i < count; ++i) {
    gc_mark_val(snap, vals[i]);
  }
}

// Counts memory that the heap took for objects or what's inside them
static inline void heap_add(Snap* snap, size_t size) {
  snap->num_bytes_alloced += size;
  snap->gc_stats.allocated += size;
}

static inline SObject* gc_alloc(Snap* snap, size_t size) {
  SObject* obj;
  int c;
  if (!GC_POOL || size > GC_MAX_SLOT) {
    SnapPage* page = (SnapPage*)malloc(sizeof(SnapPage) + size);
    page->slot_size = size;
    page->num_slots = 1;
    gc_add_page(snap, page);
    return page_slot(page, 0);
  }
  c = slot_size_index[(size + 15) / 16];
  if (!snap->free_slots[c]) gc_new_page(snap, c);
  obj = snap->free_slots[c];
  snap->free_slots[c] = ((SFreeSlot*)obj)->next;
  return obj;
}

static inline SObject* gc_new(Snap* snap, uint8_t type, size_t size) {
  SObject* obj;
  assert(size > sizeof(SObject));
  if (__builtin_expect(snap->alloc_profile, 0)) profile_alloc(snap, type, size);
  if (snap->gc_manual) {
    // The program collects with snap_gc_step()
  } else if (snap->gc_phase != GC_IDLE) {
    // Pay for allocations with collection work, so that the collection
    // finishes before the heap grows much
    snap->gc_debt += size;
    if (snap->gc_debt >= GC_STEP_SIZE) {
      size_t left = 0;
      gc_pause(snap, false, snap->gc_debt * GC_STEP_MUL / sizeof(SValue), &left);
      snap->gc_debt = left * sizeof(SValue) / GC_STEP_MUL; // What the budget left over
    }
  } else if (snap->num_bytes_alloced + size > snap->next_gc) {
    gc_pause(snap, true, 0, NULL);
  }
  obj = gc_alloc(snap, size);
  obj->type = type;
  obj->mark = snap->gc_unmarked ^ 1; // New objects survive the collection they're made in
  obj->read_only = false;
  obj->macro = false;
  heap_add(snap, size);
  return obj;
}

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

// A keyword called as a function: (:k x) is (get x :k), (:k x d) is
// (get x :k d)
static inline bool key_call(Snap* snap, SValue key, const SValue* args, int num_args, SValue* result) {
  SValue get_args[3];
  if (num_args < 1 || num_args > 2) {
    *result = create_obj(snap_err_new_format(snap, "snap_arity_error", "Expected (:%s m) or (:%s m default)",
                                             as_key(key)->data, as_key(key)->data));
    return false;
  }
  get_args[0] = args[0];
  get_args[1] = key;
  if (num_args == 2) get_args[2] = args[1];
  return builtin_get(snap, get_args, num_args + 1, result);
}

// The i-th live entry (its key, then its value), in insertion order, or NULL
// if there isn't one. After a deletion it first closes the gaps (which keeps
// the order), so walking a map by index stays constant time per entry.
static inline SValue* map_entry_at(Snap* snap, SMap* map, SnapInt i) {
  if (i < 0 || i >= map->len) return NULL;
  if (map->used != map->len) map_rebuild(snap, map);
  return &map->entries[2 * i];
}

// Sets *f to a number as a float, checking its kind once. Returns false if it
// isn't a number.
static inline bool num_val(const SValue* v, SnapFloat* f) {
  if (is_imm_float(*v)) *f = snap_imm_float_val(*v);
  else if (is_int(*v)) *f = (SnapFloat)as_int(*v);
  else if (is_obj_type(*v, STYPE_FLOAT)) *f = ((SFloat*)as_obj(*v))->f;
  else return false;
  return true;
}

static inline int seq_length(SValue seq) {
  int n = 0;
  if (is_vec(seq)) return as_vec(seq)->len;
  for (; !is_nil(seq) && !is_empty(seq); seq = as_cons(seq)->rest) n++;
  return n;
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

static inline SObject* anchor(Snap* snap, SObject* obj) {
  *mvec_push(&snap->anchors, SObject*) = obj;
  return obj;
}

static inline void release(Snap* snap) {
  mvec_pop(&snap->anchors);
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

#endif // SNAP_INTERNAL_H
