#ifndef SNAP_GC_H_H
#define SNAP_GC_H_H

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

typedef struct Span_ {
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
} Span;

typedef struct {
  MList free;
  MList full;
  size_t num_pages;
  size_t size;
  uint16_t num_slab_entries;
  bool is_slab;
} Bin;

typedef struct {
  Bin bins[NUM_SIZE_CLASSES];
  Span* huge;
  MList free_spans;
  Span** span_map;
  Span* spans;
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

#endif // SNAP_GC_H_H
