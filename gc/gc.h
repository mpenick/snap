#ifndef SNAP_GC_H_H
#define SNAP_GC_H_H

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "mstructs.h"

#define ZU(x) ((size_t)x)

#define LG_NUM_GROUPS 2
#define LG_QUANTUM 4
#define LG_MIN_SIZE LG_QUANTUM
#define LG_VADDR_SIZE 48
#define LG_PAGE_SIZE 12

#define NUM_GROUPS (ZU(1) << LG_NUM_GROUPS)
#define PAGE_SIZE (ZU(1) << LG_PAGE_SIZE)
#define NUM_SIZE_CLASSES (NUM_GROUPS * (LG_VADDR_SIZE + LG_PAGE_SIZE - LG_QUANTUM - 1))

#define MAX_SLAB_ENTRIES (PAGE_SIZE >> LG_MIN_SIZE)

typedef unsigned long Bitmap;

// FIXME
#ifdef __x86_64__
#define BITMAP_WORD_NUM_BITS 64
#define LG_BITMAP_WORD_NUM_BITS 6
#else
#define BITMAP_WORD_NUM_BITS 32
#define LG_BITMAP_WORD_NUM_BITS 5
#endif

#define BITMAP_WORD_NUM_BITS_MASK (BITMAP_WORD_NUM_BITS - 1)

#define SLAB_BITMAP_NUM_WORDS (MAX_SLAB_ENTRIES >> LG_BITMAP_WORD_NUM_BITS)

struct GCObject_;
typedef struct GCObject_ GCObject;

// GC Header:
// TTTT TTTT TTTT TTTT NNNN NNNN NNNN NNNN NNNN NNNN NNNN NNNN NNNN NNNN NNNN NNCG
// G = is gray?
// C = is container?
// N = num entries
// T = type
typedef uint64_t GCHeader;

#define GC_HEADER_BITS 64
#define GC_HEADER_IS_GRAY_BITS 1
#define GC_HEADER_IS_CONTAINER_BITS 1
#define GC_HEADER_TYPE_BITS 16
#define GC_HEADER_NUM_ENTRIES_BITS (64 - GC_HEADER_IS_GRAY_BITS - GC_HEADER_IS_CONTAINER_BITS - GC_HEADER_TYPE_BITS)

#define GC_HEADER_MASK(size) (((GCHeader)1 << size) - 1)
#define GC_HEADER_IS_GRAY_MASK GC_HEADER_MASK(GC_HEADER_IS_GRAY_BITS)
#define GC_HEADER_IS_CONTAINER_MASK GC_HEADER_MASK(GC_HEADER_IS_CONTAINER_BITS)
#define GC_HEADER_TYPE_MASK GC_HEADER_MASK(GC_HEADER_TYPE_BITS)
#define GC_HEADER_NUM_ENTRIES_MASK GC_HEADER_MASK(GC_HEADER_NUM_ENTRIES_BITS)

#define GC_HEADER_NUM_ENTRIES_SHIFT (GC_HEADER_IS_GRAY_BITS + GC_HEADER_IS_CONTAINER_BITS)
#define GC_HEADER_TYPE_SHIFT (GC_HEADER_NUM_ENTRIES_SHIFT + GC_HEADER_NUM_ENTRIES_BITS)

static inline bool gc_header_is_gray(GCHeader header) {
  return header & (GCHeader)1;
}

static inline GCHeader gc_header_set_is_gray(GCHeader header, bool is_gray) {
  if (is_gray) {
    header |= (GCHeader)1;
  } else {
    header &= ~GC_HEADER_IS_GRAY_MASK;
  }
  return header;
}

static inline bool gc_header_is_container(GCHeader header) {
  return (header >> GC_HEADER_IS_GRAY_BITS) & (GCHeader)1;
}

static inline GCHeader gc_header_set_is_container(GCHeader header, bool is_container) {
  if (is_container) {
    header |= (((GCHeader)(is_container ? 1 : 0)) << GC_HEADER_IS_GRAY_BITS);
  } else {
    GCHeader mask = ~(GC_HEADER_IS_CONTAINER_MASK << GC_HEADER_IS_GRAY_BITS);
    header &= mask;
  }
  return header;
}

static inline int64_t gc_header_num_entries(GCHeader header) {
  return (header >> GC_HEADER_NUM_ENTRIES_SHIFT) & GC_HEADER_NUM_ENTRIES_MASK;
}

static inline GCHeader gc_header_set_num_entries(GCHeader header, int64_t num_entries) {
  GCHeader mask = ~(GC_HEADER_NUM_ENTRIES_MASK << GC_HEADER_NUM_ENTRIES_SHIFT);
  header &= mask;
  header |= ((((GCHeader)num_entries) & GC_HEADER_NUM_ENTRIES_MASK) << GC_HEADER_NUM_ENTRIES_SHIFT);
  return header;
}

static inline uint16_t gc_header_type(GCHeader header) {
  return (header >> GC_HEADER_TYPE_SHIFT) & GC_HEADER_TYPE_MASK;
}

static inline GCHeader gc_header_set_type(GCHeader header, uint16_t type) {
  GCHeader mask = ~(GC_HEADER_TYPE_MASK << GC_HEADER_TYPE_SHIFT);
  header &= mask;
  header |= ((((GCHeader)type) & GC_HEADER_TYPE_MASK) << GC_HEADER_TYPE_SHIFT);
  return header;
}

typedef uintptr_t GCTagged;

static inline bool gc_tagged_is_object(GCTagged tagged) {
  return tagged & (GCTagged)1;
}

static inline bool gc_tagged_is_integer(GCTagged tagged) {
  return !gc_tagged_is_object(tagged);
}

static inline GCTagged gc_tagged_from_object(void* obj) {
  return ((GCTagged)obj << 1) | 1;
}

static inline GCTagged gc_tagged_from_integer(int64_t value) {
  return ((GCTagged)value << 1) | 0;
}

static inline int64_t gc_tagged_to_integer(GCTagged tagged) {
  assert(gc_tagged_is_integer(tagged));
  return ((int64_t)((intptr_t)(tagged))) >> 1;
}

static inline GCTagged gc_tagged_add(GCTagged a, GCTagged b) {
  return gc_tagged_from_integer(gc_tagged_to_integer(a) + gc_tagged_to_integer(b));
}

static inline GCTagged gc_tagged_sub(GCTagged a, GCTagged b) {
  return gc_tagged_from_integer(gc_tagged_to_integer(a) - gc_tagged_to_integer(b));
}

static inline void* gc_tagged_to_object(GCTagged tagged) {
  assert(gc_tagged_is_object(tagged));
  return (void*)(tagged >> 1);
}

struct GCObject_ {
  GCHeader header;
};

typedef struct {
  GCObject base;
  GCTagged entries[1];
} GCContainer;

typedef GCContainer GCVector;

typedef struct {
  MList list;
  void* ptr;
  union {
    struct {
      Bitmap small_alloced[SLAB_BITMAP_NUM_WORDS];
      Bitmap small_marked[SLAB_BITMAP_NUM_WORDS];
    };
    struct {
      bool large_marked;
    };
  };
  size_t num_pages;
  uint8_t size_index;
  uint16_t slab_entry_count;
  bool is_free;
} GCSpan;

typedef struct {
  MList free;
  MList full;
  size_t num_pages;
  size_t size;
  uint16_t num_slab_entries;
  bool is_slab;
} GCBin;

typedef struct {
  GCBin bins[NUM_SIZE_CLASSES];
  GCSpan* huge;
  MList free_spans;
  GCSpan** span_map;
  GCSpan* spans;
  void* pages;
  size_t page_count;
  void* mem;
} GC;

static inline char* to_binary(size_t value, char* buf) {
  for (int b = 0; b < 64; ++b) {
      buf[b] = ((value >> (63 - b)) & 0x1) ? '1' : '0';
  }
  buf[64] = '\0';
  return buf;
}

size_t bitmap_alloc_size(size_t num_bits);
void bitmap_init(Bitmap* bitmap, size_t num_bits);

static inline void bitmap_set(Bitmap* bitmap, size_t index) {
  size_t i = index >> LG_BITMAP_WORD_NUM_BITS;
  bitmap[i] ^= (((Bitmap)0x1) << (index & BITMAP_WORD_NUM_BITS_MASK));
}

static inline void bitmap_unset(Bitmap* bitmap, size_t index) {
  size_t i = index >> LG_BITMAP_WORD_NUM_BITS;
  bitmap[i] |= (((Bitmap)0x1) << (index & BITMAP_WORD_NUM_BITS_MASK));
}

static inline bool bitmap_is_set(Bitmap* bitmap, size_t index) {
  size_t i = index >> LG_BITMAP_WORD_NUM_BITS;
  return !(bitmap[i] & ((Bitmap)0x1) << (index & BITMAP_WORD_NUM_BITS_MASK));
}

static inline bool bitmap_is_full(Bitmap* bitmap, size_t num_bits) {
  size_t num_words = (num_bits + (BITMAP_WORD_NUM_BITS - 1)) >> LG_BITMAP_WORD_NUM_BITS;
  for (size_t i = 0; i < num_words; ++i) {
    if (bitmap[i] != 0) {
      return false;
    }
  }
  return true;
}

static inline size_t bitmap_sfu(Bitmap* bitmap) {
  size_t i = 0;
  size_t bit;
  while ((bit = (size_t)__builtin_ffsl(bitmap[i])) == 0) {
    i++;
  }
  size_t index = (i << LG_BITMAP_WORD_NUM_BITS) + (bit - 1);
  bitmap_set(bitmap, index);
  return index;
}

bool gc_init(GC* gc, size_t heap_size);

void* gc_alloc(GC* gc, size_t size);

void gc_dalloc(GC* gc, void* ptr);

void gc_collect(GC* gc);

GCObject* gc_object_new(GC* gc, size_t size);

GCContainer* gc_container_new(GC* gc, size_t num_entries);

/*
GCVector* gc_vector_new(GC* gc, size_t capacity) {
  GCVector* vec = gc_container_new(gc, 2);
  vec->entries[0] = gc_tagged_from_object(gc_container_new(gc, capacity));
  vec->entries[1] = gc_tagged_from_integer(0);
  return vec;
}

void gc_vector_check_size(GC* gc, GCVector* vec) {
  GCContainer* items = (GCContainer*)gc_tagged_to_object(vec->entries[0]);
  size_t capacity = gc_header_num_entries(items->base.header);
  size_t size = gc_tagged_to_integer(vec->entries[1]);
  if (size >= capacity) {
    // TODO: Anchor vec
    GCContainer* new_items = gc_container_new(gc, (capacity < 4096) ? 4 : 2);
    memcpy(new_items->entries, items, capacity * sizeof(GCTagged));
    vec->entries[0] = gc_tagged_from_object(new_items);
  }
}

void gc_vector_push(GC* gc, GCVector* vec, GCTagged tagged) {
  GCContainer* items = (GCContainer*)gc_tagged_to_object(vec->entries[0]);
  size_t capacity = gc_header_num_entries(items->base.header);
  size_t size = gc_tagged_to_integer(vec->entries[1]);
  if (size >= capacity) {
    // TODO: Anchor vec
    GCContainer* new_items = gc_container_new(gc, (capacity < 4096) ? 4 : 2);
    memcpy(new_items->entries, items, capacity * sizeof(GCTagged));
    vec->entries[0] = gc_tagged_from_object(new_items);
  }
  size++;
  items->entries[size] = tagged;
  vec->entries[1] = gc_tagged_from_integer(size);
}

GCTagged gc_vector_back(GCVector* vec) {
  GCContainer* items = (GCContainer*)gc_tagged_to_object(vec->entries[0]);
  size_t size = gc_tagged_to_integer(vec->entries[1]);
  return items->entries[size - 1];
}

void gc_vector_pop(GCVector* vec) {
  size_t size = gc_tagged_to_integer(vec->entries[1]);
  if (size > 0) {
    vec->entries[1] = gc_tagged_from_integer(size - 1);
  }
}
*/

#endif // SNAP_GC_H_H
