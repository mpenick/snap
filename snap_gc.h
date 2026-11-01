// The collector (snap_gc.c): its tuning, allocation, marking, the write
// barrier and anchors. gc_new() and the marking helpers are inlined.
#ifndef SNAP_SNAP_GC_H
#define SNAP_SNAP_GC_H

#include "snap.h"
#include <assert.h>
#include <stdlib.h>
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

enum {
  GC_IDLE,
  GC_MARK,
  GC_SWEEP
};

// Mark value of objects that are never freed (symbols, keywords and types). Others
// are 0 or 1, see gc_unmarked.
#define GC_FIXED 2

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

// Snapshot-at-the-beginning write barrier: while marking, a heap value about
// to be overwritten or removed is marked first. So everything reachable when
// the collection started gets marked, however the program changes the heap.
// Stores to the stack don't need it (it's marked when the collection starts),
// and neither do stores that only add values.
#define gc_barrier(snap, old) \
  do { if ((snap)->gc_phase == GC_MARK && is_obj(old)) gc_mark(snap, as_obj(old)); } while (0)

size_t code_extra_bytes(SCode* code);
int gc_free(Snap* snap, SObject* obj);
__attribute__((cold, noinline)) void profile_alloc(Snap* snap, uint8_t type, size_t size);
void gc_add_page(Snap* snap, SnapPage* page);
void gc_new_page(Snap* snap, int c);
bool gc_pause(Snap* snap, bool start, size_t work, size_t* left);

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

static inline SObject* anchor(Snap* snap, SObject* obj) {
  *mvec_push(&snap->anchors, SObject*) = obj;
  return obj;
}

static inline void release(Snap* snap) {
  mvec_pop(&snap->anchors);
}

#endif // SNAP_SNAP_GC_H
