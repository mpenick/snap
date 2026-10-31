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
#include <time.h>

// The collector is incremental mark and sweep. A collection starts once the
// heap has grown to GC_HEAP_GROWTH times what the last one found live, and by
// at least GC_MIN_STEP bytes. It then runs in steps: every GC_STEP_SIZE bytes
// allocated does GC_STEP_MUL units of work (one is marking a value or
// sweeping an object) per 8 bytes, so it keeps ahead of the program. A step
// marks at most GC_CHUNK values of one object.
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

static const uint16_t slot_sizes[SNAP_NUM_SLOT_SIZES] = { 16, 32, 48, 64, 96, 128, 192, 256 };

// Index into slot_sizes for an object of n bytes, by (n + 15) / 16
static const uint8_t slot_size_index[GC_MAX_SLOT / 16 + 1] = {
  0, 0, 1, 2, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7
};

typedef struct {
  SOBJECT_FIELDS
  SObject* next;
} SFreeSlot;

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
  XX(INT) \
  XX(MIN) \
  XX(MAX) \
  XX(CLOSURE) \
  XX(CLOSE_UPVALS) \
  XX(RETURN) \
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
  if (is_float(*val)) {
    return hash_float(as_float(*val)); // By value, so that 0.0 and -0.0 match
  }
  if (!is_obj(*val)) {
    return (int)(val->bits ^ (val->bits >> 32)); // Ints, nil, bools, forms and cfuncs
  }
  switch (obj->type) {
    // Hashed by contents so interning can look them up with a STempStr
    case STYPE_SYM:
      return hash_chars(((SSym*)obj)->data, ((SSym*)obj)->len);
    case STYPE_STR:
      return hash_chars(((SStr*)obj)->data, ((SStr*)obj)->len);
    case STYPE_KEY:
      return hash_chars(((SKeyword*)obj)->data, ((SKeyword*)obj)->len);
    case STYPE_TEMPSTR:
      return hash_chars(((STempStr*)obj)->data, ((STempStr*)obj)->len);
    // The rest match snap_compare(): errors by kind, lists, vectors, maps and
    // structs by contents
    case STYPE_ERR:
      return snap_hash(&((SErr*)obj)->err);
    case STYPE_CONS: {
      SCons* l;
      unsigned h = 1;
      for (l = (SCons*)obj; l; l = as_cons(l->rest)) h = h * 31 + snap_hash(&l->first);
      return (int)h;
    }
    case STYPE_VEC: {
      SVec* v = (SVec*)obj;
      unsigned h = 1;
      int i;
      for (i = 0; i < v->len; ++i) h = h * 31 + snap_hash(snap_vec_at(v, i));
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
    case STYPE_STRUCT: {
      SStruct* st = (SStruct*)obj;
      unsigned h = hash_pointer(st->stype);
      int i;
      for (i = 0; i < st->stype->num_fields; ++i) h = h * 31 + snap_hash(&st->vals[i]);
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
    case STYPE_CONS: {
      // By contents, element by element
      SCons* l1 = as_cons(*val1), * l2 = as_cons(*val2);
      int c;
      for (; l1 && l2; l1 = as_cons(l1->rest), l2 = as_cons(l2->rest)) {
        if ((c = snap_compare(&l1->first, &l2->first)) != 0) return c;
      }
      return compare(l1 != NULL, l2 != NULL);
    }
    case STYPE_VEC: {
      // By contents, element by element
      SVec* v1 = as_vec(*val1), * v2 = as_vec(*val2);
      int i, c;
      for (i = 0; i < v1->len && i < v2->len; ++i) {
        if ((c = snap_compare(snap_vec_at(v1, i), snap_vec_at(v2, i))) != 0) return c;
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
    case STYPE_STRUCT: {
      // By type, then field by field
      SStruct* s1 = as_struct(*val1), * s2 = as_struct(*val2);
      int i, c;
      if (s1->stype != s2->stype) return compare((uintptr_t)s1->stype, (uintptr_t)s2->stype);
      for (i = 0; i < s1->stype->num_fields; ++i) {
        if ((c = snap_compare(&s1->vals[i], &s2->vals[i])) != 0) return c;
      }
      return 0;
    }
    default:
      // Everything else, including interned symbols and keywords, by identity
      return compare(val1->bits, val2->bits);
  }
}

static SValue exec(Snap* snap);

static void print_val(FILE* out, SValue val);
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

// Counts memory that the heap took for objects or what's inside them
static inline void heap_add(Snap* snap, size_t size) {
  snap->num_bytes_alloced += size;
  snap->gc_stats.allocated += size;
}

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
    case STYPE_VEC: {
      SVec* vec = (SVec*)obj;
      int k;
      snap->num_bytes_alloced -= sizeof(SVec) + snap_vec_cap(vec) * sizeof(SValue);
      free(vec->data);
      if (vec->segs) {
        for (k = 0; k < vec->num_segs; ++k) free(vec->segs[k]);
        free(vec->segs);
      }
      break;
    }
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
    case STYPE_TYPE:
      snap->num_bytes_alloced -= sizeof(SType) + ((SType*)obj)->num_fields * sizeof(SValue);
      break;
    case STYPE_STRUCT:
      snap->num_bytes_alloced -= sizeof(SStruct) + ((SStruct*)obj)->num_vals * sizeof(SValue);
      break;
    case STYPE_METHOD:
      snap->num_bytes_alloced -= sizeof(SMethod);
      break;
    default:
      assert(0 && "Not a GC object");
      break;
  }
}

static void gc_add_page(Snap* snap, SnapPage* page) {
  page->next = snap->pages;
  snap->pages = page;
  // A sweep on the first page stays on it (new pages hold no garbage)
  if (snap->gc_sweep == &snap->pages) snap->gc_sweep = &page->next;
}

// Threads a new page's slots onto the free list for slot size c, first slot
// first
static void gc_new_page(Snap* snap, int c) {
  uint32_t size = slot_sizes[c], i, n = (GC_PAGE_SIZE - sizeof(SnapPage)) / size;
  SnapPage* page = (SnapPage*)malloc(sizeof(SnapPage) + n * size);
  page->slot_size = size;
  page->num_slots = n;
  gc_add_page(snap, page);
  for (i = n; i-- > 0;) {
    SFreeSlot* slot = (SFreeSlot*)page_slot(page, i);
    slot->type = SLOT_FREE;
    slot->next = snap->free_slots[c];
    snap->free_slots[c] = (SObject*)slot;
  }
}

static SObject* gc_alloc(Snap* snap, size_t size) {
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

// Symbols, keywords and types are GC_FIXED, so they never get past the first
// line
static void gc_mark(Snap* snap, SObject* obj) {
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

static void gc_mark_val(Snap* snap, SValue val) {
  if (is_obj(val)) gc_mark(snap, as_obj(val));
}

static void gc_mark_vals(Snap* snap, SValue* vals, int count) {
  int i;
  for (i = 0; i < count; ++i) {
    gc_mark_val(snap, vals[i]);
  }
}

// Snapshot-at-the-beginning write barrier: while marking, a heap value about
// to be overwritten or removed is marked first. So everything reachable when
// the collection started gets marked, however the program changes the heap.
// Stores to the stack don't need it (it's marked when the collection starts),
// and neither do stores that only add values.
#define gc_barrier(snap, old) \
  do { if ((snap)->gc_phase == GC_MARK && is_obj(old)) gc_mark(snap, as_obj(old)); } while (0)

// Picks the next chunk [*start, *end) of up to GC_CHUNK of count children,
// from gray->pos, and moves gray->pos past it (or to -1 when it's the last)
static void gc_chunk(SnapGray* gray, int count, int* start, int* end) {
  *start = gray->pos < count ? gray->pos : count; // A vector can shrink
  *end = count - *start > GC_CHUNK ? *start + GC_CHUNK : count;
  gray->pos = *end < count ? *end : -1;
}

// Marks the next chunk of vals. Returns the work done.
static int gc_scan_vals(Snap* snap, SnapGray* gray, SValue* vals, int count) {
  int start, end;
  gc_chunk(gray, count, &start, &end);
  gc_mark_vals(snap, vals + start, end - start);
  return end - start + 1;
}

// The index after the end of the segment holding vector index i
static int vec_seg_end(SVec* vec, int i) {
  int k = 63 - __builtin_clzll((uint64_t)(i - vec->cap0) + SNAP_VEC_SEG) - SNAP_VEC_SEG_BITS;
  return vec->cap0 + (SNAP_VEC_SEG << (k + 1)) - SNAP_VEC_SEG;
}

// Marks the children of a gray object, a chunk at a time for the ones that
// can be big. Vectors and maps can change between chunks: positions are
// indexes, removed values go through the barrier, and map_rebuild() marks
// the entries it moves.
static int gc_scan(Snap* snap, SnapGray* gray) {
  SObject* obj = gray->obj;
  int i;
  switch (obj->type) {
    case STYPE_VEC: {
      SVec* vec = (SVec*)obj;
      int start, end, run;
      gc_chunk(gray, vec->len, &start, &end);
      for (i = start; i < end; i = run) { // A run of values in one array at a time
        run = i < vec->cap0 ? vec->cap0 : vec_seg_end(vec, i);
        if (run > end) run = end;
        gc_mark_vals(snap, snap_vec_at(vec, i), run - i);
      }
      return end - start + 1;
    }
    case STYPE_MAP:
      return gc_scan_vals(snap, gray, ((SMap*)obj)->entries, 2 * ((SMap*)obj)->used); // Deleted keys aren't objects
    case STYPE_CODE:
      return gc_scan_vals(snap, gray, ((SCode*)obj)->constants, ((SCode*)obj)->num_constants);
    case STYPE_ERR:
      gc_mark_val(snap, ((SErr*)obj)->err);
      gc_mark_val(snap, ((SErr*)obj)->msg);
      break;
    case STYPE_CONS:
      gc_mark_val(snap, ((SCons*)obj)->first);
      gc_mark_val(snap, ((SCons*)obj)->rest);
      break;
    case STYPE_CLOSURE:
      gc_mark(snap, (SObject*)((SClosure*)obj)->code);
      for (i = 0; i < ((SClosure*)obj)->num_upvals; ++i) {
        gc_mark(snap, (SObject*)((SClosure*)obj)->upvals[i]);
      }
      gray->pos = -1;
      return i + 1;
    case STYPE_UPVAL:
      gc_mark_val(snap, *((SUpval*)obj)->value);
      break;
    case STYPE_STRUCT: // Its type is never freed
      return gc_scan_vals(snap, gray, ((SStruct*)obj)->vals, ((SStruct*)obj)->stype->num_fields);
    case STYPE_METHOD:
      gc_mark(snap, (SObject*)((SMethod*)obj)->table);
      break;
    default:
      assert(0 && "Not a valid gray object");
      break;
  }
  gray->pos = -1;
  return 2;
}

static void gc_mark_fn_states(Snap* snap);

// Starts a collection by flipping which mark value means unmarked, so that
// every object is unmarked without touching them, then marks the roots. The
// stack is marked all at once here, which is what lets stack stores skip the
// barrier. Symbols (and so globals) are marked in steps instead.
static void gc_start(Snap* snap) {
  SObject** obj;
  SnapFrame* frame;
  SUpval* upval;

  snap->gc_phase = GC_MARK;
  snap->gc_unmarked ^= 1;
  snap->gc_sym = 0;
  snap->gc_separated = false;
  snap->gc_debt = 0;
  snap->gc_live = snap->num_bytes_alloced; // Less what the sweep frees

  mvec_foreach(&snap->anchors, obj) {
    gc_mark(snap, *obj);
  }
  gc_mark_vals(snap, snap->pins.vitems, snap->pins.vsize);
  gc_mark_vals(snap, snap->tobefnz.vitems, snap->tobefnz.vsize); // Waiting for their finalizers

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
}

// Once marking has reached everything, moves the structs with finalizers that
// it didn't reach from finobj to tobefnz, in the order they were made, and
// marks them. They (and what they reach) live until their finalizers have run,
// which happens at the VM's next safe point, as allocation can't run snap
// code. Like Lua, this goes through finobj in one step. Returns the work done.
static size_t gc_separate(Snap* snap) {
  int i, kept = 0, n = snap->finobj.vsize, first = snap->tobefnz.vsize;
  for (i = 0; i < n; ++i) {
    SObject* obj = snap->finobj.vitems[i];
    if (obj->mark == snap->gc_unmarked) *mvec_push(&snap->tobefnz, SValue) = create_object(obj);
    else snap->finobj.vitems[kept++] = obj;
  }
  snap->finobj.vsize = kept;
  // Marking them after moving them all finalizes a struct that's only
  // reachable from another one's too, as in Lua
  for (i = first; i < snap->tobefnz.vsize; ++i) gc_mark_val(snap, snap->tobefnz.vitems[i]);
  if (snap->tobefnz.vsize > first) {
    snap->fin_pending = true;
    snap->reentered = true; // Gets the VM to look at fin_pending after its next C call
  }
  snap->gc_separated = true;
  return n + 1;
}

// Does about `work` units of marking or sweeping. Returns true when it
// finishes the collection.
static bool gc_step(Snap* snap, size_t work) {
  size_t done = 0;
  while (done < work) {
    if (snap->gc_phase == GC_MARK) {
      if (snap->gc_sym < snap->symbols.hcapacity) {
        // Symbols and keywords are never freed (GC_FIXED), but symbols hold
        // the globals. A resize of the table restarts this (see snap_sym_new()).
        SnapHashEntry* entry = &snap->symbols.hentries[snap->gc_sym++];
        if (!is_undef(entry->hkey)) gc_mark_val(snap, as_sym(entry->hkey)->value);
        done++;
      } else if (snap->gray.vsize > 0) {
        SnapGray gray = snap->gray.vitems[--snap->gray.vsize];
        done += gc_scan(snap, &gray);
        if (gray.pos >= 0) *mvec_push(&snap->gray, SnapGray) = gray;
      } else if (!snap->gc_separated) {
        done += gc_separate(snap); // Can gray more objects, so marking goes on
      } else {
        snap->gc_phase = GC_SWEEP;
        snap->gc_sweep = &snap->pages;
        snap->gc_sweep_slot = 0;
      }
    } else if (snap->gc_phase == GC_SWEEP) {
      // Objects made during the sweep are marked, wherever they go
      SnapPage* page = *snap->gc_sweep;
      SObject* obj;
      if (!page) {
        // Grow from the live estimate rather than the heap size, which
        // includes everything allocated during the collection
        snap->gc_phase = GC_IDLE;
        snap->next_gc = snap->gc_live * GC_HEAP_GROWTH;
        if (snap->next_gc < snap->num_bytes_alloced + GC_MIN_STEP) {
          snap->next_gc = snap->num_bytes_alloced + GC_MIN_STEP;
        }
        return true;
      }
      obj = page_slot(page, snap->gc_sweep_slot);
      done++;
      if (obj->type != SLOT_FREE && obj->mark == snap->gc_unmarked) {
        size_t before = snap->num_bytes_alloced;
        gc_free(snap, obj);
        snap->gc_live -= before - snap->num_bytes_alloced;
        if (page->num_slots == 1) { // An object's own page
          *snap->gc_sweep = page->next;
          free(page);
          continue;
        } else {
          int c = slot_size_index[page->slot_size / 16];
          obj->type = SLOT_FREE;
          ((SFreeSlot*)obj)->next = snap->free_slots[c];
          snap->free_slots[c] = obj;
        }
      }
      if (++snap->gc_sweep_slot == page->num_slots) {
        snap->gc_sweep = &page->next;
        snap->gc_sweep_slot = 0;
      }
    } else {
      break;
    }
  }
  return false;
}

// Starts a collection and/or does a step of one, timing it for the stats
static bool gc_pause(Snap* snap, bool start, size_t work) {
  SnapGCStats* stats = &snap->gc_stats;
  struct timespec t0, t1;
  bool done = false;
  double t;
  clock_gettime(CLOCK_MONOTONIC, &t0);
  if (start) gc_start(snap);
  if (work > 0) done = gc_step(snap, work);
  clock_gettime(CLOCK_MONOTONIC, &t1);
  t = (double)(t1.tv_sec - t0.tv_sec) + (double)(t1.tv_nsec - t0.tv_nsec) * 1e-9;
  stats->recent[stats->steps % SNAP_GC_RECENT] = t;
  stats->steps++;
  stats->pause_total += t;
  if (t > stats->pause_max) stats->pause_max = t;
  if (done) stats->collections++;
  return done;
}

bool snap_gc_step(Snap* snap, size_t work) {
  return gc_pause(snap, snap->gc_phase == GC_IDLE, work);
}

static SObject* gc_new(Snap* snap, uint8_t type, size_t size) {
  SObject* obj;
  assert(size > sizeof(SObject));
  if (snap->gc_manual) {
    // The program collects with snap_gc_step()
  } else if (snap->gc_phase != GC_IDLE) {
    // Pay for allocations with collection work, so that the collection
    // finishes before the heap grows much
    snap->gc_debt += size;
    if (snap->gc_debt >= GC_STEP_SIZE) {
      gc_pause(snap, false, snap->gc_debt * GC_STEP_MUL / sizeof(SValue));
      snap->gc_debt = 0;
    }
  } else if (snap->num_bytes_alloced + size > snap->next_gc) {
    gc_pause(snap, true, 0);
  }
  obj = gc_alloc(snap, size);
  obj->type = type;
  obj->mark = snap->gc_unmarked ^ 1; // New objects survive the collection they're made in
  obj->read_only = false;
  obj->macro = false;
  heap_add(snap, size);
  return obj;
}

/* Objects */

SValue create_float(Snap* snap, SnapFloat f) {
  SValue val;
  SFloat* n;
  if (snap_imm_float(f, &val)) return val;
  n = (SFloat*)gc_new(snap, STYPE_FLOAT, sizeof(SFloat));
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
  int capacity;
  SValue* existing = snap_hash_get_str(&snap->symbols, name, len);
  SSym* s;
  if (existing) {
    return as_sym(*existing);
  }
  s = (SSym*)gc_new(snap, STYPE_SYM, sizeof(SSym) + len + 1);
  s->mark = GC_FIXED;
  s->value = create_undef();
  s->len = len;
  memcpy(s->data, name, len + 1);
  capacity = snap->symbols.hcapacity;
  snap_hash_put(&snap->symbols, create_obj(s), create_obj(s));
  if (snap->symbols.hcapacity != capacity) snap->gc_sym = 0; // Entries moved, so mark them again
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
  k->mark = GC_FIXED;
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

// Appends a cell to a list being built and returns the slot for its value.
// The list is anchored from its first cell until list_done(), so the caller
// can allocate (even to make the value) before filling the slot.
typedef struct {
  SCons* head;
  SCons* tail;
} SnapList;

static SValue* list_add(Snap* snap, SnapList* list) {
  SCons* cell = snap_cons_new(snap);
  if (list->head) {
    list->tail->rest = create_obj(cell);
  } else {
    list->head = (SCons*)snap_anchor(snap, (SObject*)cell);
  }
  list->tail = cell;
  return &cell->first;
}

static SValue list_done(Snap* snap, SnapList* list) {
  if (!list->head) return create_empty();
  snap_release(snap);
  return create_obj(list->head);
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

SVec* snap_vec_new(Snap* snap, int cap) {
  SVec* v = (SVec*)gc_new(snap, STYPE_VEC, sizeof(SVec));
  v->num_segs = 0;
  v->len = 0;
  v->cap0 = cap;
  v->data = cap > 0 ? (SValue*)malloc(cap * sizeof(SValue)) : NULL;
  v->segs = NULL;
  heap_add(snap, cap * sizeof(SValue));
  return v;
}

// Adds a segment when it's full: it never copies values, and the segment
// pointers it copies are at most about 30. Doesn't allocate GC objects, so it
// never collects.
void snap_vec_push(Snap* snap, SVec* vec, SValue val) {
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
static void vec_shrink(Snap* snap, SVec* vec) {
  int n = vec->num_segs;
  if (n >= 2 && vec->len <= vec->cap0 + (SNAP_VEC_SEG << (n - 2)) - SNAP_VEC_SEG) {
    free(vec->segs[n - 1]);
    vec->num_segs--;
    snap->num_bytes_alloced -= ((size_t)SNAP_VEC_SEG << (n - 1)) * sizeof(SValue);
  }
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
      // A collection marking this map a chunk at a time could miss moved
      // entries, so mark them
      gc_barrier(snap, map->entries[2 * i]);
      gc_barrier(snap, map->entries[2 * i + 1]);
      entries[2 * len] = map->entries[2 * i];
      entries[2 * len + 1] = map->entries[2 * i + 1];
      len++;
    }
  }
  free(map->entries);
  free(map->index);
  snap->num_bytes_alloced -= map_bytes(map->cap);
  heap_add(snap, map_bytes(cap));
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
  } else {
    gc_barrier(snap, map->entries[2 * e + 1]);
  }
  map->entries[2 * e + 1] = val;
}

bool snap_map_del(Snap* snap, SMap* map, SValue key) {
  int e = map_find(map, key);
  if (e < 0) return false;
  gc_barrier(snap, map->entries[2 * e]);
  gc_barrier(snap, map->entries[2 * e + 1]);
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

// fields is a vector of keywords, or NULL for a built-in type
static SType* type_new(Snap* snap, const char* name, SVec* fields) {
  int i, n = fields ? fields->len : 0;
  SType* t = (SType*)gc_new(snap, STYPE_TYPE, sizeof(SType) + n * sizeof(SValue));
  t->mark = GC_FIXED;
  t->is_struct = fields != NULL;
  t->num_fields = n;
  t->name = snap_sym_new(snap, name); // Interned, so it can't be collected
  t->gc = create_nil();
  for (i = 0; i < n; ++i) t->fields[i] = *snap_vec_at(fields, i);
  return t;
}

SType* snap_type_of(Snap* snap, SValue val) {
  return is_struct(val) ? as_struct(val)->stype : snap->types[snap_type(val)];
}

// Makes an instance of a struct type with the values of its fields in order.
// Doesn't run any code, so the type and values only need to be rooted.
static bool struct_new(Snap* snap, SType* type, const SValue* args, int num_args, SValue* result) {
  SStruct* st;
  if (!type->is_struct) {
    *result = create_obj(snap_err_new_format(snap, "snap_type_error", "%s isn't a struct type", type->name->data));
    return false;
  }
  if (num_args != type->num_fields) {
    *result = create_obj(snap_err_new_format(snap, "snap_arity_error", "%s has %d fields",
                                             type->name->data, type->num_fields));
    return false;
  }
  st = (SStruct*)gc_new(snap, STYPE_STRUCT, sizeof(SStruct) + num_args * sizeof(SValue));
  st->stype = type;
  st->num_vals = num_args;
  if (num_args > 0) memcpy(st->vals, args, num_args * sizeof(SValue));
  if (!is_nil(type->gc) && !snap->gc_closing) *mvec_push(&snap->finobj, SObject*) = (SObject*)st;
  *result = create_obj(st);
  return true;
}

// The slot of a struct's field, or NULL if it doesn't have it
static SValue* struct_field(SStruct* st, SValue key) {
  int i;
  for (i = 0; i < st->stype->num_fields; ++i) {
    if (st->stype->fields[i].bits == key.bits) return &st->vals[i];
  }
  return NULL;
}

// The function a method call runs, picked by its first argument's type, or
// NULL if it doesn't have one
static SValue* method_find(Snap* snap, SMethod* m, const SValue* args, int num_args) {
  return num_args > 0 ? snap_map_get(m->table, create_obj(snap_type_of(snap, args[0]))) : NULL;
}

static SErr* method_error(Snap* snap, SMethod* m, const SValue* args, int num_args) {
  if (num_args == 0) return snap_err_new_format(snap, "snap_arity_error", "Method %s needs an argument", m->name->data);
  return snap_err_new_format(snap, "snap_type_error", "No method %s for %s", m->name->data,
                             snap_type_of(snap, args[0])->name->data);
}

SObject* snap_anchor(Snap* snap, SObject* obj) {
  *mvec_push(&snap->anchors, SObject*) = obj;
  return obj;
}

void snap_release(Snap* snap) {
  mvec_pop(&snap->anchors);
}

// Pins are roots, marked when a collection starts. A value pinned during the
// mark needs no barrier: it was either reachable then, so it gets marked, or
// it's new, and new objects are already marked.
SValue snap_pin(Snap* snap, SValue val) {
  if (is_obj(val)) *mvec_push(&snap->pins, SValue) = val;
  return val;
}

// Removes the latest pin of val, by identity, moving the last pin into its place
void snap_unpin(Snap* snap, SValue val) {
  int i;
  for (i = snap->pins.vsize - 1; i >= 0; --i) {
    if (snap->pins.vitems[i].bits == val.bits) {
      snap->pins.vitems[i] = snap->pins.vitems[--snap->pins.vsize];
      return;
    }
  }
}

void snap_set_gc(Snap* snap, SType* type, SValue fn) {
  assert(type->is_struct);
  snap_unpin(snap, type->gc);
  type->gc = snap_pin(snap, fn);
}

void snap_run_finalizers(Snap* snap) {
  if (snap->fin_running || snap->c_depth >= SNAP_MAX_C_DEPTH) return;
  snap->fin_running = true;
  while (snap->tobefnz.vsize > 0) {
    // Popped and pinned, so it's kept for the call, then left for the
    // collector unless the finalizer kept it. It's never finalized again.
    SValue obj = snap->tobefnz.vitems[--snap->tobefnz.vsize], r;
    SType* type = as_struct(obj)->stype;
    if (is_nil(type->gc)) continue; // Taken away since
    snap_pin(snap, obj);
    r = snap_call(snap, type->gc, &obj, 1);
    if (is_err(r)) {
      // There's no one to return the error to, so it's a warning, as in Lua
      fflush(stdout); // After what the program printed before it
      fprintf(stderr, "snap: finalizer for %s failed: ", type->name->data);
      print_val(stderr, r);
      fputc('\n', stderr);
    }
    snap_unpin(snap, obj);
  }
  snap->fin_pending = false;
  snap->fin_running = false;
}

void snap_def(Snap* snap, const char* name, SValue val) {
  SSym* sym = snap_sym_new(snap, name);
  gc_barrier(snap, sym->value);
  sym->value = val;
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

#define is_int_p(v) is_int(*(v))
#define is_num_p(v) (is_int_p(v) || is_float(*(v)))
#define num_p(v) (is_int_p(v) ? (SnapFloat)as_int(*(v)) : as_float(*(v)))

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
  SValue val = create_float(snap, f);
  if (dest) {
    frame->stack_base[dest - 1] = val;
  } else {
    *frame->stack_top++ = val;
  }
}

// Whether a float truncates to a 63-bit int (false for NaN)
#define in_int_range(f) ((f) >= (SnapFloat)SNAP_INT_MIN && (f) < -(SnapFloat)SNAP_INT_MIN)

// Sets *f to a number as a float, checking its kind once. Returns false if it
// isn't a number.
static inline bool num_val(const SValue* v, SnapFloat* f) {
  if (is_imm_float(*v)) *f = snap_imm_float_val(*v);
  else if (is_int(*v)) *f = (SnapFloat)as_int(*v);
  else if (is_obj_type(*v, STYPE_FLOAT)) *f = ((SFloat*)as_obj(*v))->f;
  else return false;
  return true;
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

// Built-ins that the NTH, COUNT, GET, INT, MIN and MAX instructions compare
// globals with
bool builtin_nth(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_min(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_max(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_count(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_get(Snap* snap, const SValue* args, int num_args, SValue* result);
static bool builtin_int(Snap* snap, const SValue* args, int num_args, SValue* result);

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
// function whose last parameter takes the rest of its arguments. Returns the
// new top. The list is built from the end, each cell going in the slot of the
// value it took, so the GC sees all of it.
static SValue* pack_rest(Snap* snap, SValue* top, int count) {
  SValue* slot;
  *top = create_empty(); // The callable's slot, which is free
  mvec_back(&snap->frames).stack_top = top + 1;
  for (slot = top - 1; slot >= top - count; --slot) {
    SCons* cell = snap_cons_new(snap);
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
      OP(GET) {
        SValue* val;
        a = stack - 2;
        b = stack - 1;
        if (builtin_global(builtin_get)) {
          if (is_struct(*a) && (val = struct_field(as_struct(*a), *b))) {
            *a = *val;
            stack--;
            DISPATCH();
          }
          if (is_map(*a)) {
            val = snap_map_get(as_map(*a), *b);
            *a = val ? *val : create_nil();
            stack--;
            DISPATCH();
          }
        }
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

// 'x, ~x and ~@x read as (quote x), (unquote x) and (splice x)
static SValue parse_wrap(Snap* snap, SnapLex* lex, int form) {
  SCons* sexpr = (SCons*)snap_anchor(snap, (SObject*)snap_cons_new(snap));
  sexpr->first = create_form(form);
  sexpr->rest = create_obj(snap_cons_new(snap));
  as_cons(sexpr->rest)->first = parse_expr(snap, lex, snap_lex_next_token(lex));
  snap_release(snap);
  return create_obj(sexpr);
}

bool builtin_list(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_concat(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_vec(Snap* snap, const SValue* args, int num_args, SValue* result);

// Whether expr is (form x)
static bool is_wrapped(SValue expr, int form) {
  return is_cons(expr) && !is_empty(expr) && is_form(as_cons(expr)->first) &&
         as_form(as_cons(expr)->first) == form;
}

static SValue quasi(Snap* snap, SValue expr);

// Code that makes a list of the quasiquoted elements: (list a b) or, if
// some are spliced in, (concat (list a) b). The functions themselves go in
// the code rather than their names, so locals can't hide them.
static SValue quasi_list(Snap* snap, SCons* elems) {
  SnapList code = { NULL, NULL };
  SCons* curr;
  SValue* slot;
  bool splice = false;
  for (curr = elems; curr; curr = as_cons(curr->rest)) {
    if (is_wrapped(curr->first, TK_SPLICE)) splice = true;
  }
  *list_add(snap, &code) = create_cfunc(splice ? builtin_concat : builtin_list);
  for (curr = elems; curr; curr = as_cons(curr->rest)) {
    slot = list_add(snap, &code);
    if (is_wrapped(curr->first, TK_SPLICE)) {
      *slot = nth(as_cons(curr->first), 1);
    } else if (splice) {
      SnapList part = { NULL, NULL };
      *list_add(snap, &part) = create_cfunc(builtin_list);
      *list_add(snap, &part) = create_nil();
      part.tail->first = quasi(snap, curr->first);
      *slot = list_done(snap, &part);
    } else {
      *slot = quasi(snap, curr->first);
    }
  }
  return list_done(snap, &code);
}

// Expands `expr into code that builds it, with ~x replaced by the value of x
// and ~@x by the elements of x. Symbols and special forms are quoted, and
// other atoms are their own value.
static SValue quasi(Snap* snap, SValue expr) {
  int i;
  SnapList code = { NULL, NULL };
  switch (snap_type(expr)) {
    case STYPE_CONS:
      if (is_empty(expr)) return expr;
      if (is_wrapped(expr, TK_UNQUOTE)) return nth(as_cons(expr), 1);
      if (is_wrapped(expr, TK_SPLICE)) {
        raise(snap, "snap_parse_error", "~@ must be in a list or vector");
      }
      return quasi_list(snap, as_cons(expr));
    case STYPE_VEC: {
      SVec* vec = as_vec(expr);
      SVec* out;
      for (i = 0; i < vec->len; ++i) {
        if (is_wrapped(*snap_vec_at(vec, i), TK_SPLICE)) break;
      }
      if (i < vec->len) { // A splice: (vec (concat ...)) of the elements as a list
        SValue elems;
        for (i = 0; i < vec->len; ++i) *list_add(snap, &code) = *snap_vec_at(vec, i);
        elems = list_done(snap, &code);
        snap_anchor(snap, as_obj(elems));
        code.head = NULL;
        *list_add(snap, &code) = create_cfunc(builtin_vec);
        *list_add(snap, &code) = create_nil();
        code.tail->first = quasi_list(snap, as_cons(elems));
        expr = list_done(snap, &code);
        snap_release(snap);
        return expr;
      }
      out = (SVec*)snap_anchor(snap, (SObject*)snap_vec_new(snap, vec->len));
      for (i = 0; i < vec->len; ++i) {
        SValue val = quasi(snap, *snap_vec_at(vec, i));
        snap_vec_push(snap, out, val);
      }
      snap_release(snap);
      return create_obj(out);
    }
    case STYPE_MAP: {
      SMap* map = as_map(expr);
      SMap* out = (SMap*)snap_anchor(snap, (SObject*)snap_map_new(snap));
      for (i = 0; i < map->used; ++i) { // A literal has no deleted entries
        SValue key = quasi(snap, map->entries[2 * i]), val;
        if (is_obj(key)) snap_anchor(snap, as_obj(key));
        val = quasi(snap, map->entries[2 * i + 1]);
        snap_map_put(snap, out, key, val);
        if (is_obj(key)) snap_release(snap);
      }
      snap_release(snap);
      return create_obj(out);
    }
    case STYPE_SYM:
    case STYPE_FORM:
      *list_add(snap, &code) = create_form(TK_QUOTE);
      *list_add(snap, &code) = expr;
      return list_done(snap, &code);
    default:
      return expr;
  }
}

static SValue parse_int(Snap* snap, SnapLex* lex) {
  char* end;
  long long i;
  errno = 0;
  i = strtoll(lex->val, &end, strchr(lex->val, 'x') ? 16 : 10);
  if (errno == ERANGE || i < SNAP_INT_MIN || i > SNAP_INT_MAX) {
    raise(snap, "snap_parse_error", "Integer literal is out of range on line %d", lex->line);
  }
  return create_int(i);
}

// Replaces \n, \t, \r, \\ and \" in a string literal, in place
static void parse_escapes(Snap* snap, SnapLex* lex) {
  char* in, * out;
  for (in = out = lex->val; *in; ++in) {
    if (*in != '\\') {
      *out++ = *in;
      continue;
    }
    switch (*++in) {
      case 'n': *out++ = '\n'; break;
      case 't': *out++ = '\t'; break;
      case 'r': *out++ = '\r'; break;
      case '\\': case '"': *out++ = *in; break;
      default:
        raise(snap, "snap_parse_error", "Unknown escape '\\%c' on line %d", *in, lex->line);
    }
  }
  *out = '\0';
}

// Counts a level of nesting in the reader or compiler toward SNAP_MAX_C_DEPTH.
// The caller takes it back off when it's done, or compile_forms() does after
// a raise.
static void enter(Snap* snap, const char* err) {
  if (++snap->c_depth > SNAP_MAX_C_DEPTH) raise(snap, err, "Too deeply nested");
}

static SValue parse_value(Snap* snap, SnapLex* lex, int token);

static SValue parse_expr(Snap* snap, SnapLex* lex, int token) {
  SValue val;
  enter(snap, "snap_parse_error");
  val = parse_value(snap, lex, token);
  snap->c_depth--;
  return val;
}

static SValue parse_value(Snap* snap, SnapLex* lex, int token) {
  switch (token) {
    case '(':
      return parse_sexpr(snap, lex);
    case '[':
      return parse_vec(snap, lex);
    case '{':
      return parse_map(snap, lex);
    case '\'':
      return parse_wrap(snap, lex, TK_QUOTE);
    case '~':
      return parse_wrap(snap, lex, TK_UNQUOTE);
    case TK_SPLICE:
      return parse_wrap(snap, lex, TK_SPLICE);
    case '`': {
      SValue expr = parse_expr(snap, lex, snap_lex_next_token(lex)), code;
      if (is_obj(expr)) snap_anchor(snap, as_obj(expr));
      code = quasi(snap, expr);
      if (is_obj(expr)) snap_release(snap);
      return code;
    }
    case TK_INT:
      return parse_int(snap, lex);
    case TK_FLOAT:
      return create_float(snap, atof(lex->val));
    case TK_STR:
      parse_escapes(snap, lex);
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
    case TK_DEFMACRO:
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
    case TK_RECOVER:
    case TK_ADD: case TK_SUB: case TK_MUL: case TK_DIV: case TK_MOD:
    case TK_LT: case TK_LE: case TK_GT: case TK_GE: case TK_EQ:
    case TK_NOT: case TK_AND: case TK_OR:
      return create_form(token);
    case TK_EOF:
      raise(snap, "snap_eof_error", "Premature end of file on line %d", lex->line);
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
  Snap* snap;
  struct SnapFnState_* up;
  struct { MVEC_FIELDS(SnapInst); } insts;
  SnapVec constants;
  SnapLocal locals[MAX_LOCALS];
  int num_locals;
  int max_locals;
  uint32_t upvals[MAX_UPVALS]; // (index << 1) | islocal
  int num_upvals;
  int num_args;
  bool variadic;   // The last parameter takes the rest of the arguments
  bool top;        // Code from snap_compile(), which can't see the locals of
                   // the function whose macro is compiling it
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

static SCode* code_new(Snap* snap, SnapFnState* fs) {
  SCode* c = (SCode*)gc_new(snap, STYPE_CODE, sizeof(SCode));
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
      case ADD: val = create_float(fs->snap, x + y); break;
      case SUB: val = create_float(fs->snap, x - y); break;
      case MUL: val = create_float(fs->snap, x * y); break;
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
    if (op == POP && (last == LOAD_LOCAL || last == LOAD_CLOSED ||
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
        *inst = INST(call_op(fs, INST_ARG(*inst), arg), INST_ARG(*inst) << 8 | arg);
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
    case LOAD_GLOBAL: case LOAD_LOCAL: case LOAD_CLOSED:
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

static int add_global_constant(Snap* snap, SnapFnState* fs, SValue val);

static int add_constant(Snap* snap, SnapFnState* fs, SValue val) {
  int i;
  // Floats by identity: compared, 0.0 equals -0.0, and NaN equals anything
  if (is_float(val)) return add_global_constant(snap, fs, val);
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

// let and loop bindings can shadow a name bound earlier in the same form
static int add_local(Snap* snap, SnapFnState* fs, SSym* name, bool shadow) {
  int i;
  SnapLocal* local;
  for (i = fs->num_locals - 1; !shadow && i >= 0 && fs->locals[i].depth == fs->depth; --i) {
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
  if (fs->top) return -1;
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

// A constant global's value, as a constant of the function. These are
// matched by identity rather than contents, so that two constant vectors that
// happen to be equal stay two vectors.
static int add_global_constant(Snap* snap, SnapFnState* fs, SValue val) {
  int i;
  for (i = 0; i < fs->constants.vsize; ++i) {
    if (fs->constants.vitems[i].bits == val.bits) return i;
  }
  if (i > MAX_ARG) {
    raise(snap, "snap_compile_error", "Too many constants");
  }
  *mvec_push(&fs->constants, SValue) = val;
  return i;
}

static void load_variable(Snap* snap, SnapFnState* fs, SSym* name) {
  int index;
  if ((index = resolve_local(fs, name)) >= 0) {
    emit(fs, LOAD_LOCAL, index);
  } else if ((index = resolve_upval(snap, fs, name)) >= 0) {
    emit(fs, LOAD_CLOSED, index);
  } else if (name->read_only) {
    // A constant (see set-const!): its value goes in the code in its place
    emit(fs, LOAD_CONSTANT, add_global_constant(snap, fs, name->value));
  } else {
    emit(fs, LOAD_GLOBAL, add_constant(snap, fs, create_obj(name)));
  }
}

static void store_global(Snap* snap, SnapFnState* fs, SSym* name) {
  if (name->read_only) {
    raise(snap, "snap_compile_error", "%s is a constant", name->data);
  }
  emit(fs, STORE_GLOBAL, add_constant(snap, fs, create_obj(name)));
}

static void store_variable(Snap* snap, SnapFnState* fs, SSym* name) {
  int index;
  if ((index = resolve_local(fs, name)) >= 0) {
    emit(fs, STORE_LOCAL, index);
  } else if ((index = resolve_upval(snap, fs, name)) >= 0) {
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

static void compile_def(Snap* snap, SnapFnState* fs, SCons* args) {
  SSym* name;
  if (list_length(args) != 2 || !is_sym(args->first)) {
    raise(snap, "snap_compile_error", "Expected (def <id> <expr>)");
  }
  name = as_sym(args->first);
  compile(snap, fs, nth(args, 1), false);
  if (fs->depth > 0) {
    emit(fs, STORE_LOCAL, add_local(snap, fs, name, false));
  } else {
    store_global(snap, fs, name);
  }
  emit(fs, LOAD_NIL, 0); /* TODO: Hack */
}

bool builtin_make_macro(Snap* snap, const SValue* args, int num_args, SValue* result);
static void compile_fn(Snap* snap, SnapFnState* up, SCons* args);

// (defmacro name [params] body...) compiles like (def name (fn [params]
// body...)), with the function marked as a macro
static void compile_defmacro(Snap* snap, SnapFnState* fs, SCons* args) {
  if (!args || !is_sym(args->first)) {
    raise(snap, "snap_compile_error", "Expected (defmacro <id> [<params>] <expr>...)");
  }
  if (fs->depth > 0 || !fs->top) {
    raise(snap, "snap_compile_error", "defmacro must be at the top level");
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
    if (resolve_local(fs, as_sym(name)) >= 0) return false;
    if (fs->top) break;
  }
  val = as_sym(name)->value;
  if (!is_obj(val) || !as_obj(val)->macro) return false;
  *macro = val;
  return true;
}

// Calls a macro with the forms it was given, unevaluated, and returns the
// form it makes: an error if it panics or returns one
static SValue expand(Snap* snap, SValue macro, SCons* args) {
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
    raise(snap, "snap_compile_error", "Expected (set! <id> <expr>)");
  }
  compile(snap, fs, nth(args, 1), false);
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
  enter(snap, "snap_compile_error");
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
    compile(snap, fs, cond, false);
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
  SValue then = nth(args, 1), other = nth(args, 2);
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
    raise(snap, "snap_compile_error", "Expected (if <cond> <then> <else>)");
  }
  compile_branches(snap, fs, args, tail, false);
}

static void compile_fn(Snap* snap, SnapFnState* up, SCons* args) {
  int i;
  SSym* amp;
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

  // [a b & more] puts the arguments after the first two in a list, `more`
  amp = snap_sym_new(snap, "&");
  for (i = 0; i < params->len; ++i) {
    SValue param = *snap_vec_at(params, i);
    if (!is_sym(param)) {
      raise(snap, "snap_compile_error", "Expected symbol");
    }
    if (as_sym(param) == amp) {
      if (i != params->len - 2) {
        raise(snap, "snap_compile_error", "Expected one parameter after &");
      }
      fs->variadic = true;
      continue;
    }
    add_local(snap, fs, as_sym(param), false);
  }
  fs->num_args = fs->num_locals;

  fs->fn_loop.top = 0;
  fs->fn_loop.base = 0;
  fs->fn_loop.count = fs->num_args;
  fs->fn_loop.closes = contains_form(args->rest, TK_FN);
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

// Bindings alternate names and values: (let [a 1 b 2] ...)
static void compile_let(Snap* snap, SnapFnState* fs, SCons* args, bool loop, bool tail) {
  int i, base, count;
  SVec* bindings;
  SnapLoop recur_target;

  if (!args || !is_vec(args->first)) {
    raise(snap, "snap_compile_error", "Expected binding vector");
  }
  if (!as_cons(args->rest)) {
    raise(snap, "snap_compile_error", "Expected expressions");
  }

  bindings = as_vec(args->first);
  count = bindings->len / 2;
  if (bindings->len % 2 != 0) {
    raise(snap, "snap_compile_error", "Expected bindings [<id> <expr> ...]");
  }
  for (i = 0; i < count; ++i) {
    if (!is_sym(*snap_vec_at(bindings, 2 * i))) {
      raise(snap, "snap_compile_error", "Expected bindings [<id> <expr> ...]");
    }
  }

  // In order, so each value can use the names before it. Scopes inside a
  // value free their slots, so the bindings get consecutive slots (which
  // recur relies on).
  begin_scope(fs);
  base = fs->num_locals;
  for (i = 0; i < count; ++i) {
    compile(snap, fs, *snap_vec_at(bindings, 2 * i + 1), false);
    emit(fs, STORE_LOCAL, add_local(snap, fs, as_sym(*snap_vec_at(bindings, 2 * i)), true));
  }
  assert(fs->num_locals == base + count);

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
  arg = nth(args, slot - target->base);
  if (!is_cons(arg) || !is_form(as_cons(arg)->first)) return -1;
  operands = as_cons(as_cons(arg)->rest);
  if (list_length(operands) != 2) return -1;
  if (as_form(as_cons(arg)->first) == TK_ADD && is_int(operands->first)) {
    k = operands->first;
    v = nth(operands, 1);
  } else if (as_form(as_cons(arg)->first) == TK_ADD || as_form(as_cons(arg)->first) == TK_SUB) {
    v = operands->first;
    k = nth(operands, 1);
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
    raise(snap, "snap_compile_error", "recur must be inside a fn or loop");
  }
  if (!tail) {
    raise(snap, "snap_compile_error", "recur must be in tail position");
  }
  if (list_length(args) != target->count) {
    raise(snap, "snap_compile_error", "Invalid number of arguments");
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
    raise(snap, "snap_compile_error", "Expected (recover <fn>)");
  }
  compile(snap, fs, args->first, false);
  emit(fs, RECOVER, 0);
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
  if (opcode == SUB && list_length(args) == 1) { // (- x) is (- 0 x)
    compile_binary(snap, fs, opcode, create_int(0), args->first);
    return;
  }
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

// Compiles an expression whose value isn't used, leaving nothing on the stack.
// Then an if (or a when or cond, once expanded) doesn't need to push nil for
// a missing branch, then pop it.
static void compile_effect(Snap* snap, SnapFnState* fs, SValue expr) {
  SCons* sexpr = snap_type(expr) == STYPE_CONS ? as_cons(expr) : NULL;
  SCons* args = sexpr ? as_cons(sexpr->rest) : NULL;
  SValue macro, form;
  enter(snap, "snap_compile_error");
  if (sexpr && is_sym(sexpr->first) && find_macro(fs, sexpr->first, &macro)) {
    form = expand(snap, macro, args);
    if (is_err(form)) {
      snap->cause = as_err(form);
      longjmp(snap->jmp, 1);
    }
    if (is_obj(form)) snap_anchor(snap, as_obj(form));
    compile_effect(snap, fs, form);
    if (is_obj(form)) snap_release(snap);
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

static void compile_sexpr(Snap* snap, SnapFnState* fs, SCons* sexpr, bool tail) {
  SCons* args = as_cons(sexpr->rest);
  SValue macro, form;
  switch (snap_type(sexpr->first)) {
    case STYPE_SYM:
      if (find_macro(fs, sexpr->first, &macro)) {
        form = expand(snap, macro, args);
        if (is_err(form)) {
          snap->cause = as_err(form);
          longjmp(snap->jmp, 1);
        }
        if (is_obj(form)) snap_anchor(snap, as_obj(form));
        compile(snap, fs, form, tail);
        if (is_obj(form)) snap_release(snap);
        break;
      }
      compile_call(snap, fs, sexpr);
      break;
    case STYPE_CONS:
    case STYPE_CFUNC: // From quasiquote
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
        case TK_DEFMACRO:
          compile_defmacro(snap, fs, args);
          break;
        case TK_UNQUOTE:
        case TK_SPLICE:
          raise(snap, "snap_compile_error", "~ and ~@ must be in a quasiquote (`)");
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
static bool op_value(int token, SValue* val);
static const char* form_name(int token);

static void compile(Snap* snap, SnapFnState* fs, SValue expr, bool tail) {
  int stack = fs->stack;
  SValue val;
  enter(snap, "snap_compile_error");
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
        raise(snap, "snap_compile_error", "%s can't be used as a value", form_name(as_form(expr)));
      }
      load_constant(snap, fs, val);
      break;
    default:
      raise(snap, "snap_compile_error", "Unexpected type");
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
static SValue compile_forms(Snap* snap, SnapLex* lex, bool all) {
  SValue expr, result;
  SnapFnState* fs;
  SnapFnState* up = snap->fn_state;
  SCode* code;
  bool first = true;
  int num_anchors = snap->anchors.vsize, c_depth = snap->c_depth;
  jmp_buf jmp;

  memcpy(jmp, snap->jmp, sizeof(jmp_buf));
  if (setjmp(snap->jmp) > 0) {
    while (snap->fn_state != up) fn_state_free(snap);
    snap->c_depth = c_depth;
    result = create_obj(snap->cause);
    goto done;
  }

  fs = fn_state_new(snap);
  fs->top = true;
  while ((all || first) && parse(snap, lex, &expr)) {
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

static void lex_init(SnapLex* lex, const char* str) {
  lex->buf = str;
  lex->buf_size = strlen(lex->buf);
  lex->p = lex->buf;
  lex->line = 1;
}

SValue snap_compile(Snap* snap, const char* str) {
  SnapLex lex;
  lex_init(&lex, str);
  return compile_forms(snap, &lex, true);
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
  if (is_cfunc(fn) || is_type(fn)) {
    result = create_nil();
    // On failure result is the error
    if (is_cfunc(fn)) as_cfunc(fn)(snap, args, num_args, &result);
    else struct_new(snap, as_type(fn), args, num_args, &result);
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
    rest = list_done(snap, &list); // Rooted again by the frame before anything allocates
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

// Runs each form as soon as it's compiled, so that a macro can be used by
// the forms after it. Stops at an error.
SValue snap_exec(Snap* snap, const char* str) {
  SnapLex lex;
  SValue fn, result = create_nil();
  int num_anchors = snap->anchors.vsize;
  lex_init(&lex, str);
  for (;;) {
    fn = compile_forms(snap, &lex, false);
    if (!is_code(fn)) break;
    snap->anchors.vsize = num_anchors; // Releases the last result
    result = snap_run(snap, fn);
    if (is_err(result)) break;
    if (is_obj(result)) snap_anchor(snap, as_obj(result));
    if (snap->fin_pending) snap_run_finalizers(snap);
  }
  snap->anchors.vsize = num_anchors;
  return is_err(fn) ? fn : result; // An error, or the last result
}

/* Printing */

void snap_print(SValue val) {
  print_val(stdout, val);
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
    case TK_DEFMACRO: return "defmacro";
    case TK_UNQUOTE: return "unquote";
    case TK_SPLICE: return "splice";
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

// Quoted and escaped, so that it reads back as the same string
static void print_str(FILE* out, SStr* s) {
  size_t i;
  fputc('"', out);
  for (i = 0; i < s->len; ++i) {
    switch (s->data[i]) {
      case '\n': fprintf(out, "\\n"); break;
      case '\t': fprintf(out, "\\t"); break;
      case '\r': fprintf(out, "\\r"); break;
      case '\\': fprintf(out, "\\\\"); break;
      case '"': fprintf(out, "\\\""); break;
      default: fputc(s->data[i], out); break;
    }
  }
  fputc('"', out);
}

static void print_val(FILE* out, SValue val) {
  int i;
  SCons* cons;
  switch (snap_type(val)) {
    case STYPE_NIL:
      fprintf(out, "nil");
      break;
    case STYPE_BOOL:
      fprintf(out, "%s", is_false(val) ? "false" : "true");
      break;
    case STYPE_INT:
      fprintf(out, "%ld", (long)as_int(val));
      break;
    case STYPE_FLOAT:
      fprintf(out, "%f", as_float(val));
      break;
    case STYPE_CFUNC:
      fprintf(out, "<cfunc %p>", (void*)as_cfunc(val));
      break;
    case STYPE_FORM:
      fprintf(out, "%s", form_name(as_form(val)));
      break;
    case STYPE_STR:
      print_str(out, as_str(val));
      break;
    case STYPE_SYM:
      fprintf(out, "%s", as_sym(val)->data);
      break;
    case STYPE_KEY:
      fprintf(out, ":%s", as_key(val)->data);
      break;
    case STYPE_ERR:
      fprintf(out, "<err ");
      print_val(out, as_err(val)->err);
      fprintf(out, " ");
      print_val(out, as_err(val)->msg);
      fprintf(out, ">");
      break;
    case STYPE_VEC:
      fprintf(out, "[");
      for (i = 0; i < as_vec(val)->len; ++i) {
        if (i > 0) fprintf(out, " ");
        print_val(out, *snap_vec_at(as_vec(val), i));
      }
      fprintf(out, "]");
      break;
    case STYPE_MAP: {
      SMap* m = as_map(val);
      bool first = true;
      fprintf(out, "{");
      for (i = 0; i < m->used; ++i) {
        if (m->entries[2 * i].bits == SV_DELETED) continue;
        if (!first) fprintf(out, " ");
        first = false;
        print_val(out, m->entries[2 * i]);
        fprintf(out, " ");
        print_val(out, m->entries[2 * i + 1]);
      }
      fprintf(out, "}");
      break;
    }
    case STYPE_CONS:
      fprintf(out, "(");
      for (cons = as_cons(val); cons != NULL; ) {
        print_val(out, cons->first);
        if (!is_cons(cons->rest)) {
          fprintf(out, " . ");
          print_val(out, cons->rest);
          break;
        }
        cons = as_cons(cons->rest);
        if (cons) fprintf(out, " ");
      }
      fprintf(out, ")");
      break;
    case STYPE_CODE:
      fprintf(out, "<fn %p>", (void*)as_obj(val));
      break;
    case STYPE_CLOSURE:
      fprintf(out, "<closure %p>", (void*)as_obj(val));
      break;
    case STYPE_TYPE:
      fprintf(out, "%s", as_type(val)->name->data);
      break;
    case STYPE_STRUCT: {
      SStruct* st = as_struct(val);
      fprintf(out, "#%s{", st->stype->name->data);
      for (i = 0; i < st->stype->num_fields; ++i) {
        if (i > 0) fprintf(out, " ");
        print_val(out, st->stype->fields[i]);
        fprintf(out, " ");
        print_val(out, st->vals[i]);
      }
      fprintf(out, "}");
      break;
    }
    case STYPE_METHOD:
      fprintf(out, "<method %s>", as_method(val)->name->data);
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
      case LOAD_LOCAL: case STORE_LOCAL: case LOAD_CLOSED: case STORE_CLOSED:
      case CALL: case CLOSE_UPVALS:
        printf(" %d", arg);
        break;
      case CALL_GLOBAL: case NTH: case COUNT: case GET: case INT: case MIN: case MAX:
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
  if (is_cons(args[0])) {
    *result = create_int(list_length(as_cons(args[0])));
  } else if (is_vec(args[0])) {
    *result = create_int(as_vec(args[0])->len);
  } else if (is_map(args[0])) {
    *result = create_int(as_map(args[0])->len);
  } else {
    return fail(snap, result, "snap_type_error", "Expected a vector, map or list");
  }
  return true;
}

// (nth <vec> <index>) or (nth <list> <index>)
bool builtin_nth(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args == 2 && is_cons(args[0]) && is_int(args[1])) {
    if (as_int(args[1]) < 0 || as_int(args[1]) >= list_length(as_cons(args[0]))) {
      return fail(snap, result, "snap_index_error", "Index out of range");
    }
    *result = nth(as_cons(args[0]), as_int(args[1]));
    return true;
  }
  if (!vec_args(snap, args, num_args, 2, result)) return false;
  *result = *snap_vec_at(as_vec(args[0]), as_int(args[1]));
  return true;
}

static bool field_error(Snap* snap, const SValue* args, SValue* result) {
  *result = create_obj(snap_err_new_format(snap, "snap_key_error", "%s has no field :%s",
                                           as_struct(args[0])->stype->name->data,
                                           is_key(args[1]) ? as_key(args[1])->data : "(not a keyword)"));
  return false;
}

// (put! <vec> <index> <val>), (put! <map> <key> <val>) or
// (put! <struct> <field> <val>) returns the vector, map or struct
bool builtin_put(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SValue* val;
  if (num_args == 3 && is_struct(args[0])) {
    if (!(val = struct_field(as_struct(args[0]), args[1]))) return field_error(snap, args, result);
    gc_barrier(snap, *val);
    *val = args[2];
    *result = args[0];
    return true;
  }
  if (num_args == 3 && is_map(args[0])) {
    if (as_obj(args[0])->read_only) return read_only_error(snap, result);
    snap_map_put(snap, as_map(args[0]), args[1], args[2]);
    *result = args[0];
    return true;
  }
  if (!vec_args(snap, args, num_args, 3, result)) return false;
  if (as_obj(args[0])->read_only) return read_only_error(snap, result);
  val = snap_vec_at(as_vec(args[0]), as_int(args[1]));
  gc_barrier(snap, *val);
  *val = args[2];
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
  *result = *snap_vec_at(vec, --vec->len);
  gc_barrier(snap, *result);
  vec_shrink(snap, vec);
  return true;
}

// (get <map> <key>) or (get <map> <key> <default>). The default is nil.
// (get <struct> <field>) panics if the struct doesn't have the field.
bool builtin_get(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SValue* val;
  if (num_args != 2 && num_args != 3) return arity_error(snap, result);
  if (is_struct(args[0])) {
    if (!(val = struct_field(as_struct(args[0]), args[1]))) return field_error(snap, args, result);
    *result = *val;
    return true;
  }
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
  snap_map_del(snap, as_map(args[0]), args[1]);
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

// (str <str>...) joins strings
// (str x...) joins strings, and other values as they print
bool builtin_str(Snap* snap, const SValue* args, int num_args, SValue* result) {
  char* buf = NULL;
  size_t len = 0;
  FILE* out = open_memstream(&buf, &len);
  SStr* s;
  int i;
  for (i = 0; i < num_args; ++i) {
    if (is_str(args[i])) fwrite(as_str(args[i])->data, 1, as_str(args[i])->len, out);
    else print_val(out, args[i]);
  }
  fclose(out);
  s = (SStr*)gc_new(snap, STYPE_STR, sizeof(SStr) + len + 1);
  memcpy(s->data, buf, len + 1);
  s->len = len;
  free(buf);
  *result = create_obj(s);
  return true;
}

// (compile <str>) compiles code into a function without arguments, or returns
// an error if it doesn't parse or compile, or nil if there's no code
bool builtin_compile(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  if (!is_str(args[0])) return fail(snap, result, "snap_type_error", "Expected a string");
  *result = snap_compile(snap, as_str(args[0])->data);
  return true;
}

// (gc-step <work>) does about <work> units of collection, and returns true
// if that finished a collection
bool builtin_gc_step(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  if (!is_int(args[0]) || as_int(args[0]) < 0) return fail(snap, result, "snap_type_error", "Expected a non-negative integer");
  *result = create_bool(snap_gc_step(snap, as_int(args[0])));
  return true;
}

// Sorts the latest pauses into out (by insertion, as there are at most
// SNAP_GC_RECENT). Returns how many there are.
static size_t gc_recent_sorted(const SnapGCStats* stats, double* out) {
  size_t n = stats->steps < SNAP_GC_RECENT ? stats->steps : SNAP_GC_RECENT, i, j;
  for (i = 0; i < n; ++i) {
    double t = stats->recent[i];
    for (j = i; j > 0 && out[j - 1] > t; --j) out[j] = out[j - 1];
    out[j] = t;
  }
  return n;
}

// The pth percentile (0 to 1) of n sorted times, between the two nearest
static double percentile(const double* sorted, size_t n, double p) {
  double pos = p * (double)(n - 1);
  size_t i = (size_t)pos;
  if (n == 0) return 0;
  if (i + 1 >= n) return sorted[n - 1];
  return sorted[i] + (sorted[i + 1] - sorted[i]) * (pos - (double)i);
}

// (gc-stats) is a map of what the collector has done, and the heap now
bool builtin_gc_stats(Snap* snap, const SValue* args, int num_args, SValue* result) {
  static const char* phases[] = { "idle", "mark", "sweep" };
  const SnapGCStats* stats = &snap->gc_stats;
  double sorted[SNAP_GC_RECENT];
  size_t n = gc_recent_sorted(stats, sorted);
  SMap* m;
  if (num_args != 0) return arity_error(snap, result);
  m = (SMap*)snap_anchor(snap, (SObject*)snap_map_new(snap));
#define PUT(name, val) snap_map_put(snap, m, create_obj(snap_key_new(snap, name)), val)
  PUT("heap", create_int((SnapInt)snap->num_bytes_alloced));
  PUT("next-gc", create_int((SnapInt)snap->next_gc));
  PUT("phase", create_obj(snap_key_new(snap, phases[snap->gc_phase])));
  PUT("collections", create_int((SnapInt)stats->collections));
  PUT("steps", create_int((SnapInt)stats->steps));
  PUT("allocated", create_int((SnapInt)stats->allocated));
  PUT("pause-total", create_float(snap, stats->pause_total));
  PUT("pause-max", create_float(snap, stats->pause_max));
  PUT("pause-mean", create_float(snap, stats->steps ? stats->pause_total / stats->steps : 0));
  PUT("pause-median", create_float(snap, percentile(sorted, n, 0.5)));
  PUT("pause-p75", create_float(snap, percentile(sorted, n, 0.75)));
  PUT("pause-p90", create_float(snap, percentile(sorted, n, 0.9)));
#undef PUT
  snap_release(snap);
  *result = create_obj(m);
  return true;
}

bool builtin_keys(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return map_entries(snap, args, num_args, 0, result);
}

bool builtin_vals(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return map_entries(snap, args, num_args, 1, result);
}

/* Lists, mostly for macros. nil counts as an empty list. */

static bool list_arg(Snap* snap, SValue val, SCons** list, SValue* result) {
  if (is_nil(val)) {
    *list = NULL;
    return true;
  }
  if (!is_cons(val)) return fail(snap, result, "snap_type_error", "Expected a list");
  *list = as_cons(val);
  return true;
}

bool builtin_list(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int i;
  SnapList list = { NULL, NULL };
  for (i = 0; i < num_args; ++i) *list_add(snap, &list) = args[i];
  *result = list_done(snap, &list);
  return true;
}

// (cons x list) is list with x in front
bool builtin_cons(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SCons* list;
  SCons* cell;
  if (num_args != 2) return arity_error(snap, result);
  if (!list_arg(snap, args[1], &list, result)) return false;
  cell = snap_cons_new(snap);
  cell->first = args[0];
  cell->rest = list ? args[1] : create_empty();
  *result = create_obj(cell);
  return true;
}

// The first value of a list, or nil if it's empty
bool builtin_first(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SCons* list;
  if (num_args != 1) return arity_error(snap, result);
  if (!list_arg(snap, args[0], &list, result)) return false;
  *result = list ? list->first : create_nil();
  return true;
}

// A list without its first value: () if it's empty
bool builtin_rest(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SCons* list;
  if (num_args != 1) return arity_error(snap, result);
  if (!list_arg(snap, args[0], &list, result)) return false;
  *result = list ? list->rest : create_empty();
  return true;
}

// (concat a b...) is a new list of the values in the lists or vectors
bool builtin_concat(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int i, j;
  SnapList list = { NULL, NULL };
  SCons* curr;
  for (i = 0; i < num_args; ++i) {
    if (!is_vec(args[i]) && !list_arg(snap, args[i], &curr, result)) return false;
  }
  for (i = 0; i < num_args; ++i) {
    if (is_vec(args[i])) {
      for (j = 0; j < as_vec(args[i])->len; ++j) {
        *list_add(snap, &list) = *snap_vec_at(as_vec(args[i]), j);
      }
    } else {
      for (curr = is_nil(args[i]) ? NULL : as_cons(args[i]); curr; curr = as_cons(curr->rest)) {
        *list_add(snap, &list) = curr->first;
      }
    }
  }
  *result = list_done(snap, &list);
  return true;
}

// (vec list) is a new vector of the values in a list
bool builtin_vec(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SCons* list;
  SVec* vec;
  if (num_args != 1) return arity_error(snap, result);
  if (!list_arg(snap, args[0], &list, result)) return false;
  vec = snap_vec_new(snap, list_length(list)); // The only allocation
  for (; list; list = as_cons(list->rest)) snap_vec_push(snap, vec, list->first);
  *result = create_obj(vec);
  return true;
}

bool builtin_islist(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_bool(is_cons(args[0]));
  return true;
}

bool builtin_issym(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_bool(is_sym(args[0]));
  return true;
}

/* Macros */

// Marks the function from defmacro (see compile_defmacro())
bool builtin_make_macro(Snap* snap, const SValue* args, int num_args, SValue* result) {
  as_obj(args[0])->macro = true;
  *result = args[0];
  return true;
}

// (gensym) or (gensym <prefix>) makes a new symbol, #g1, #g2..., that can't
// be the same as any in the code, for a macro's own variables
bool builtin_gensym(Snap* snap, const SValue* args, int num_args, SValue* result) {
  char name[128];
  if (num_args > 1 || (num_args == 1 && !is_str(args[0]))) {
    return fail(snap, result, "snap_type_error", "Expected (gensym <prefix>)");
  }
  snprintf(name, sizeof(name), "#%.100s%d", num_args == 1 ? as_str(args[0])->data : "g",
           ++snap->num_gensyms);
  *result = create_obj(snap_sym_new(snap, name)); // Interned, so it lives forever
  return true;
}

// (macroexpand form) expands form while it's a macro call (but not the forms
// inside it), to show what a macro makes
bool builtin_macroexpand(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int num_anchors = snap->anchors.vsize;
  SValue form, macro;
  if (num_args != 1) return arity_error(snap, result);
  form = args[0];
  while (is_cons(form) && !is_empty(form) && is_sym(as_cons(form)->first)) {
    macro = as_sym(as_cons(form)->first)->value;
    if (!is_obj(macro) || !as_obj(macro)->macro) break;
    form = expand(snap, macro, as_cons(as_cons(form)->rest));
    snap->anchors.vsize = num_anchors; // Releases the last expansion
    if (is_err(form)) {
      *result = form;
      return false;
    }
    if (is_obj(form)) snap_anchor(snap, as_obj(form));
  }
  snap->anchors.vsize = num_anchors;
  *result = form;
  return true;
}

/* Operators as values */

// What the arithmetic instructions do, folding left over two or more numbers
static bool arith(Snap* snap, int op, const SValue* args, int num_args, SValue* result) {
  SValue acc;
  int i;
  if (num_args < 2 && !(op == TK_SUB && num_args == 1)) return arity_error(snap, result);
  acc = num_args == 1 ? create_int(0) : args[0]; // (- x) is (- 0 x)
  for (i = num_args == 1 ? 0 : 1; i < num_args; ++i) {
    const SValue* a = &acc, * b = &args[i];
    if (!is_num_p(a) || !is_num_p(b)) return fail(snap, result, "snap_type_error", "Expected numbers");
    if ((op == TK_DIV || op == TK_MOD) && int_zero_divisor()) {
      return fail(snap, result, "snap_arith_error", "Division by zero");
    }
    if (is_int_p(a) && is_int_p(b)) {
      switch (op) {
        case TK_ADD: acc.bits = add_iop(a->bits, b->bits); break;
        case TK_SUB: acc.bits = sub_iop(a->bits, b->bits); break;
        case TK_MUL: acc.bits = mul_iop(a->bits, b->bits); break;
        case TK_DIV: acc.bits = div_iop(a->bits, b->bits); break;
        default: acc.bits = mod_iop(a->bits, b->bits); break;
      }
    } else {
      SnapFloat x = num_p(a), y = num_p(b), f;
      switch (op) {
        case TK_ADD: f = add_fop(x, y); break;
        case TK_SUB: f = sub_fop(x, y); break;
        case TK_MUL: f = mul_fop(x, y); break;
        case TK_DIV: f = div_fop(x, y); break;
        default: f = mod_fop(x, y); break;
      }
      acc = create_float(snap, f); // Only used by the next step, before anything else allocates
    }
  }
  *result = acc;
  return true;
}

#define compare_vals(op) \
  (is_int_p(a) && is_int_p(b) ? (SnapInt)a->bits op (SnapInt)b->bits \
   : is_num_p(a) && is_num_p(b) ? num_p(a) op num_p(b) : snap_compare(a, b) op 0)

// What the compare instructions do: true if each pair of values in a row is
static bool compare_args(Snap* snap, int op, const SValue* args, int num_args, SValue* result) {
  bool cond = true;
  int i;
  if (num_args < 2) return arity_error(snap, result);
  for (i = 1; i < num_args && cond; ++i) {
    const SValue* a = &args[i - 1], * b = &args[i];
    switch (op) {
      case TK_LT: cond = compare_vals(<); break;
      case TK_LE: cond = compare_vals(<=); break;
      case TK_GT: cond = compare_vals(>); break;
      case TK_GE: cond = compare_vals(>=); break;
      default: cond = compare_vals(==); break;
    }
  }
  *result = create_bool(cond);
  return true;
}

bool builtin_not(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_bool(is_falsy(args[0]));
  return true;
}

#define XX(name, fn, token) \
  static bool name(Snap* snap, const SValue* args, int num_args, SValue* result) { \
    return fn(snap, token, args, num_args, result); \
  }
XX(builtin_add, arith, TK_ADD) XX(builtin_sub, arith, TK_SUB) XX(builtin_mul, arith, TK_MUL)
XX(builtin_div, arith, TK_DIV) XX(builtin_mod, arith, TK_MOD)
XX(builtin_lt, compare_args, TK_LT) XX(builtin_le, compare_args, TK_LE)
XX(builtin_gt, compare_args, TK_GT) XX(builtin_ge, compare_args, TK_GE)
XX(builtin_eq, compare_args, TK_EQ)
#undef XX

// Sets *val to the C function for an operator. Returns false if the form
// isn't one.
static bool op_value(int token, SValue* val) {
  static const SCFunc ops[] = {
    builtin_add, builtin_sub, builtin_mul, builtin_div, builtin_mod,
    builtin_lt, builtin_le, builtin_gt, builtin_ge, builtin_eq
  };
  if (token == TK_NOT) {
    *val = create_cfunc(builtin_not);
  } else if (token >= TK_ADD && token <= TK_EQ) {
    *val = create_cfunc(ops[token - TK_ADD]);
  } else {
    return false;
  }
  return true;
}

/* Math */

// (sqrt x), (sin x), (cos x) and (atan2 y x) of ints or floats are floats
#define XX(name, expr) \
  static bool name(Snap* snap, const SValue* args, int num_args, SValue* result) { \
    if (num_args != 1 || !is_num_p(&args[0])) return fail(snap, result, "snap_type_error", "Expected a number"); \
    SnapFloat x = num_p(&args[0]); \
    *result = expr; \
    return true; \
  }
XX(builtin_sqrt, create_float(snap, sqrt(x))) XX(builtin_sin, create_float(snap, sin(x)))
XX(builtin_cos, create_float(snap, cos(x))) XX(builtin_float, create_float(snap, x))
XX(builtin_abs, is_int(args[0]) ? create_int(llabs(as_int(args[0]))) : create_float(snap, fabs(x)))
#undef XX

// (floor x) and (int x) (toward zero) are ints. Floats too big for one (or NaN)
// are an error.
static bool to_int(Snap* snap, const SValue* args, int num_args, bool round_down, SValue* result) {
  SnapFloat x;
  if (num_args != 1 || !num_val(&args[0], &x)) return fail(snap, result, "snap_type_error", "Expected a number");
  if (is_int(args[0])) {
    *result = args[0];
    return true;
  }
  if (round_down) x = floor(x);
  if (!in_int_range(x)) return fail(snap, result, "snap_arith_error", "Number is too big for an integer");
  *result = create_int((SnapInt)x);
  return true;
}

static bool builtin_floor(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return to_int(snap, args, num_args, true, result);
}

static bool builtin_int(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return to_int(snap, args, num_args, false, result);
}

bool builtin_atan2(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 2 || !is_num_p(&args[0]) || !is_num_p(&args[1])) {
    return fail(snap, result, "snap_type_error", "Expected (atan2 <y> <x>)");
  }
  *result = create_float(snap, atan2(num_p(&args[0]), num_p(&args[1])));
  return true;
}

// (min x y...) and (max x y...) are the smallest and largest of their
// arguments
static bool min_max(Snap* snap, int op, const SValue* args, int num_args, SValue* result) {
  int i, best = 0;
  if (num_args < 1) return arity_error(snap, result);
  for (i = 0; i < num_args; ++i) {
    if (!is_num_p(&args[i])) return fail(snap, result, "snap_type_error", "Expected numbers");
    if (op == TK_LT ? num_p(&args[i]) < num_p(&args[best]) : num_p(&args[i]) > num_p(&args[best])) best = i;
  }
  *result = args[best];
  return true;
}

bool builtin_min(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return min_max(snap, TK_LT, args, num_args, result);
}

bool builtin_max(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return min_max(snap, TK_GT, args, num_args, result);
}

// (rand) is a float from 0 up to 1, and (rand n) an int from 0 up to n
// (xorshift64*)
bool builtin_rand(Snap* snap, const SValue* args, int num_args, SValue* result) {
  static uint64_t state = 0x9e3779b97f4a7c15;
  uint64_t r;
  if (num_args > 1 || (num_args == 1 && (!is_int(args[0]) || as_int(args[0]) <= 0))) {
    return fail(snap, result, "snap_type_error", "Expected (rand) or (rand <positive int>)");
  }
  state ^= state >> 12;
  state ^= state << 25;
  state ^= state >> 27;
  r = state * 0x2545f4914f6cdd1d;
  *result = num_args == 0 ? create_float(snap, (SnapFloat)(r >> 11) / (SnapFloat)(1ull << 53))
                          : create_int((SnapInt)(r % (uint64_t)as_int(args[0])));
  return true;
}

/* Constants */

// (set-const! 'name val) makes a global that can't change (see defconst in
// stdlib.snap). Its symbol is marked read-only, and code compiled after this
// uses the value in place of the global.
bool builtin_set_const(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SSym* sym;
  if (num_args != 2 || !is_sym(args[0])) return fail(snap, result, "snap_type_error", "Expected (set-const! <name> <value>)");
  sym = as_sym(args[0]);
  if (sym->read_only) {
    *result = create_obj(snap_err_new_format(snap, "snap_type_error", "%s is already a constant", sym->data));
    return false;
  }
  gc_barrier(snap, sym->value);
  sym->value = args[1];
  sym->read_only = true;
  *result = create_nil();
  return true;
}

/* Types and methods */

bool builtin_type(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_obj(snap_type_of(snap, args[0]));
  return true;
}

// (make-type 'Name [field...]) makes a struct type (see defstruct). The fields
// can be symbols or keywords; they're keywords in the type.
bool builtin_make_type(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SVec* fields;
  int i;
  if (num_args != 2 || !is_sym(args[0]) || !is_vec(args[1])) {
    return fail(snap, result, "snap_type_error", "Expected (make-type <name> [<field>...])");
  }
  fields = (SVec*)snap_anchor(snap, (SObject*)snap_vec_new(snap, as_vec(args[1])->len));
  for (i = 0; i < as_vec(args[1])->len; ++i) {
    SValue f = *snap_vec_at(as_vec(args[1]), i);
    if (is_sym(f)) f = create_obj(snap_key_new(snap, as_sym(f)->data));
    if (!is_key(f)) {
      snap_release(snap);
      return fail(snap, result, "snap_type_error", "Expected fields to be symbols or keywords");
    }
    snap_vec_push(snap, fields, f);
  }
  *result = create_obj(type_new(snap, as_sym(args[0])->data, fields));
  snap_release(snap);
  return true;
}

// (add-method! 'name Type fn) makes fn the implementation of method name for
// Type, defining the method if it isn't yet (see defmethod)
bool builtin_add_method(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SSym* name;
  SMethod* m;
  if (num_args != 3 || !is_sym(args[0]) || !is_type(args[1])) {
    return fail(snap, result, "snap_type_error", "Expected (add-method! <name> <type> <fn>)");
  }
  name = as_sym(args[0]);
  if (is_method(name->value)) {
    m = as_method(name->value);
  } else if (is_undef(name->value)) {
    SMap* table = (SMap*)snap_anchor(snap, (SObject*)snap_map_new(snap));
    m = (SMethod*)gc_new(snap, STYPE_METHOD, sizeof(SMethod));
    m->name = name;
    m->table = table;
    name->value = create_obj(m);
    snap_release(snap);
  } else {
    *result = create_obj(snap_err_new_format(snap, "snap_type_error",
                                             "%s is already defined and isn't a method", name->data));
    return false;
  }
  snap_map_put(snap, m->table, args[1], args[2]);
  *result = create_obj(m);
  return true;
}

// (set-gc! Type f) has f called with each struct of Type made from now on,
// once nothing can reach it (see defgc). (set-gc! Type nil) takes it away.
bool builtin_set_gc(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SValue fn = num_args == 2 ? args[1] : create_nil();
  if (num_args != 2 || !is_type(args[0]) || !as_type(args[0])->is_struct ||
      !(is_nil(fn) || is_cfunc(fn) || is_code(fn) || is_closure(fn) || is_method(fn))) {
    return fail(snap, result, "snap_type_error", "Expected (set-gc! <struct type> <fn or nil>)");
  }
  snap_set_gc(snap, as_type(args[0]), fn);
  *result = args[0];
  return true;
}

// (find-method m Type) is the function method m (or the method named by the
// symbol m) runs for Type, or nil if there isn't one
bool builtin_find_method(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SValue m, * fn = NULL;
  if (num_args != 2 || !is_type(args[1])) {
    return fail(snap, result, "snap_type_error", "Expected (find-method <method> <type>)");
  }
  m = is_sym(args[0]) ? as_sym(args[0])->value : args[0];
  if (is_method(m)) fn = snap_map_get(as_method(m)->table, args[1]);
  *result = fn ? *fn : create_nil();
  return true;
}

// stdlib.snap as a string, generated by the Makefile
static const char stdlib_src[] =
#include "stdlib_snap.h"
;

void snap_init(Snap* snap) {
  snap->stack_size = 64;
  snap->stack = (SValue*)malloc(snap->stack_size * sizeof(SValue));
  snap->stack_end = snap->stack + snap->stack_size;
  mvec_init(&snap->frames, SnapFrame, 32);
  mvec_init(&snap->anchors, SObject*, 32);
  mvec_init(&snap->pins, SValue, 8);
  snap->num_bytes_alloced = 0;
  snap->next_gc = GC_MIN_STEP;
  snap->pages = NULL;
  memset(snap->free_slots, 0, sizeof(snap->free_slots));
  snap->gc_phase = GC_IDLE;
  memset(&snap->gc_stats, 0, sizeof(snap->gc_stats));
  snap->gc_unmarked = 0;
  snap->gc_manual = false;
  snap->gc_sym = 0;
  snap->gc_debt = 0;
  snap->gc_live = 0;
  snap->gc_sweep = NULL;
  snap->gc_sweep_slot = 0;
  mvec_init(&snap->gray, SnapGray, 64);
  mvec_init(&snap->finobj, SObject*, 16);
  mvec_init(&snap->tobefnz, SValue, 16);
  snap->gc_separated = false;
  snap->fin_pending = false;
  snap->fin_running = false;
  snap->gc_closing = false;
  snap->open_upvals = NULL;
  snap->fn_state = NULL;
  snap->cause = NULL;
  snap->num_gensyms = 0;
  snap->dump_code = false;
  snap->reentered = false;
  snap->c_depth = 0;
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
  snap_def_cfunc(snap, "str", builtin_str);
  snap_def_cfunc(snap, "compile", builtin_compile);
  snap_def_cfunc(snap, "gc-step", builtin_gc_step);
  snap_def_cfunc(snap, "gc-stats", builtin_gc_stats);
  snap_def_cfunc(snap, "set-const!", builtin_set_const);
  snap_def_cfunc(snap, "list", builtin_list);
  snap_def_cfunc(snap, "cons", builtin_cons);
  snap_def_cfunc(snap, "first", builtin_first);
  snap_def_cfunc(snap, "rest", builtin_rest);
  snap_def_cfunc(snap, "concat", builtin_concat);
  snap_def_cfunc(snap, "vec", builtin_vec);
  snap_def_cfunc(snap, "list?", builtin_islist);
  snap_def_cfunc(snap, "sym?", builtin_issym);
  snap_def_cfunc(snap, "gensym", builtin_gensym);
  snap_def_cfunc(snap, "macroexpand", builtin_macroexpand);
  snap_def_cfunc(snap, "type", builtin_type);
  snap_def_cfunc(snap, "make-type", builtin_make_type);
  snap_def_cfunc(snap, "add-method!", builtin_add_method);
  snap_def_cfunc(snap, "find-method", builtin_find_method);
  snap_def_cfunc(snap, "set-gc!", builtin_set_gc);
  snap_def_cfunc(snap, "sqrt", builtin_sqrt);
  snap_def_cfunc(snap, "sin", builtin_sin);
  snap_def_cfunc(snap, "cos", builtin_cos);
  snap_def_cfunc(snap, "atan2", builtin_atan2);
  snap_def_cfunc(snap, "floor", builtin_floor);
  snap_def_cfunc(snap, "int", builtin_int);
  snap_def_cfunc(snap, "float", builtin_float);
  snap_def_cfunc(snap, "abs", builtin_abs);
  snap_def_cfunc(snap, "min", builtin_min);
  snap_def_cfunc(snap, "max", builtin_max);
  snap_def_cfunc(snap, "rand", builtin_rand);

  {
    // The built-in types, which methods can be added to like struct types
    static const struct { const char* name; int types[4]; } builtin_types[] = {
      { "Nil", { STYPE_NIL } }, { "Bool", { STYPE_BOOL } }, { "Int", { STYPE_INT } },
      { "Float", { STYPE_FLOAT } }, { "Str", { STYPE_STR } }, { "Sym", { STYPE_SYM, STYPE_FORM } },
      { "Key", { STYPE_KEY } }, { "Err", { STYPE_ERR } }, { "List", { STYPE_CONS } },
      { "Vector", { STYPE_VEC } }, { "Map", { STYPE_MAP } }, { "Type", { STYPE_TYPE } },
      { "Fn", { STYPE_CFUNC, STYPE_CODE, STYPE_CLOSURE, STYPE_METHOD } },
    };
    size_t i, j;
    memset(snap->types, 0, sizeof(snap->types));
    for (i = 0; i < sizeof(builtin_types) / sizeof(builtin_types[0]); ++i) {
      SType* t = type_new(snap, builtin_types[i].name, NULL);
      for (j = 0; j < 4 && builtin_types[i].types[j]; ++j) snap->types[builtin_types[i].types[j]] = t;
      snap_def(snap, builtin_types[i].name, create_obj(t));
    }
  }

  if (is_err(snap_exec(snap, stdlib_src))) {
    fprintf(stderr, "stdlib.snap failed\n");
    abort();
  }
}

void snap_destroy(Snap* snap) {
  SnapPage* page;
  int i;
  // Every struct with a finalizer gets it run, live or not, as in lua_close().
  // Nothing collects meanwhile, and structs made by finalizers don't get one.
  snap->gc_manual = true;
  snap->gc_closing = true;
  for (i = 0; i < snap->finobj.vsize; ++i) {
    *mvec_push(&snap->tobefnz, SValue) = create_object(snap->finobj.vitems[i]);
  }
  snap->finobj.vsize = 0;
  snap_run_finalizers(snap);

  page = snap->pages;
  while (page) {
    SnapPage* next = page->next;
    uint32_t i;
    for (i = 0; i < page->num_slots; ++i) {
      SObject* obj = page_slot(page, i);
      if (obj->type != SLOT_FREE) gc_free(snap, obj);
    }
    free(page);
    page = next;
  }
  free((void*)snap->stack);
  mvec_destroy(&snap->frames);
  mvec_destroy(&snap->anchors);
  mvec_destroy(&snap->pins);
  mvec_destroy(&snap->gray);
  mvec_destroy(&snap->finobj);
  mvec_destroy(&snap->tobefnz);
  snap_hash_destroy(&snap->symbols);
  snap_hash_destroy(&snap->keywords);
  assert(snap->num_bytes_alloced == 0);
}
