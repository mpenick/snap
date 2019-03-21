#include <assert.h>

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include <sys/mman.h>

#include "mstructs.h"

#define ZU(x) ((size_t)x)

#define LG_PTR_SIZE 8
#define LG_BITMAP_WORD_SIZE 8

#define LG_NUM_GROUPS 2
#define LG_QUANTUM 4
#define LG_MIN_SIZE LG_QUANTUM
#define LG_VADDR_SIZE 48
#define LG_PAGE_SIZE 12

#define NUM_GROUPS (ZU(1) << LG_NUM_GROUPS)
#define PAGE_SIZE (ZU(1) << LG_PAGE_SIZE)
#define NUM_SIZE_CLASSES (NUM_GROUPS * (LG_VADDR_SIZE + LG_PAGE_SIZE - LG_QUANTUM - 1))

#define MAX_SLAB_ENTRIES (PAGE_SIZE >> LG_MIN_SIZE)

static inline uintptr_t round_to_alignment(uintptr_t value, uintptr_t alignment) {
   return (value + (alignment - 1)) & ~(alignment - 1);
}

typedef struct Span_ {
  struct Span_* next;
  void* ptr;
  union {
    struct {
      unsigned long long small_alloced[MAX_SLAB_ENTRIES >> LG_BITMAP_WORD_SIZE];
      unsigned long long small_marked[MAX_SLAB_ENTRIES >> LG_BITMAP_WORD_SIZE];
    };
    struct {
      bool large_marked;
    };
  };
  size_t num_pages;
  uint16_t size_index;
  bool is_free;
} Span;

static size_t gc_page_span_map_metadata_size(size_t page_count) {
  return page_count * sizeof(Span*);
}

static size_t gc_page_all_metadata_size(size_t page_count) {
  return  gc_page_span_map_metadata_size(page_count) + page_count * sizeof(Span);
}

typedef struct {
  Span* free;
  Span* full;
  size_t num_pages;
  bool is_slab;
} Bin;

typedef struct {
  Bin bins[NUM_SIZE_CLASSES];
  Span* huge;
  Span* free_spans;
  Span** page_span_map;
  Span* spans;
  void* pages;
  size_t page_count;
  void* mem;
} GC;

static size_t gc_calc_num_pages(size_t size) {
  size_t num_pages = 1;
  size_t num_entries = PAGE_SIZE / size;
  while (true) {
    if (num_pages * PAGE_SIZE ==  num_entries * size) {
      return num_pages;
    }
    num_pages++;
    num_entries = (num_pages * PAGE_SIZE) / size;
  }
}

void gc_bin_init(Bin* bin, bool is_slab, size_t num_pages) {
  bin->is_slab = is_slab;
  bin->num_pages = num_pages;
  bin->free = bin->free = NULL;
}

void gc_calc_size_classes(GC* gc) {
  size_t lg_base = LG_MIN_SIZE;
  size_t lg_delta = LG_QUANTUM;

  size_t index = 0;

  for (size_t n = 0; n < 2 * NUM_GROUPS; ++n) {
    size_t size = ((size_t)1 << lg_base) + (n << lg_delta);
    gc_bin_init(&gc->bins[index], true, gc_calc_num_pages(size));
    index++;
  }

  lg_base+=2;

  while (((size_t)1 << lg_base) + (NUM_GROUPS << lg_delta) < ((size_t)1 << LG_VADDR_SIZE) * PAGE_SIZE) {
    lg_base++;
    lg_delta++;
    for (size_t n = 1; n <= NUM_GROUPS; ++n) {
      size_t size = ((size_t)1 << lg_base) + (n << lg_delta);

      size_t next_size;
      if (n < NUM_GROUPS) {
        next_size = ((size_t)1 << lg_base) + ((n + 1) << lg_delta);
      } else {
        next_size = ((size_t)1 << (lg_base + 1)) + ((size_t)1 << (lg_delta + 1));
      }

      if (size < 4 * PAGE_SIZE) {
        size_t num_pages = gc_calc_num_pages(size);
        bool is_slab = size < PAGE_SIZE || size % PAGE_SIZE != 0;
        gc_bin_init(&gc->bins[index], is_slab, num_pages);
      } else {
        gc_bin_init(&gc->bins[index], false, size >> LG_PAGE_SIZE);
      }
      index++;
    }
  }
  assert(index == NUM_SIZE_CLASSES && "Calculated an invalid number of size classes");
}

static size_t gc_size_to_index(size_t size);

static size_t gc_ptr_to_page_index(GC* gc, void* ptr) {
  return ((uintptr_t)ptr - (uintptr_t)gc->pages) >> LG_PAGE_SIZE;
}

static Span* gc_alloc_span(GC* gc, void* ptr, Span* next, size_t num_pages, size_t size_index, bool is_free) {
  assert(gc->free_spans != NULL && "A span should alway be available");

  Span* span = gc->free_spans;
  gc->free_spans = span->next;

  // Mark first and last pages in the mapping
  size_t page_index = gc_ptr_to_page_index(gc, ptr);
  gc->page_span_map[page_index] = span;
  gc->page_span_map[page_index + num_pages - 1] = span;

  span->ptr = ptr;
  span->next = next;
  span->num_pages = num_pages;
  span->size_index = size_index;
  span->is_free = is_free;

  return span;
}

static void gc_dalloc_span(GC* gc, Span* span) {
  span->next = gc->free_spans;
  gc->free_spans = span;
}


static void gc_dalloc(GC* gc, void* ptr, size_t size) {
  size_t index = gc_size_to_index(size);
  Bin* bin = &gc->bins[index];
  if (bin->is_slab) {
    // Free slab bit
    // Check to see if slab is empty
    // If empty then move to first non-slab bin of the correct page size
  } else {
    // FIXME: Deal with removing from the "full" list!!!

    size_t page_index = gc_ptr_to_page_index(gc, ptr);
    Span* span = gc->page_span_map[page_index];
    uintptr_t next_page_index = page_index + span->num_pages;
    bool is_merged = false;

    if (page_index >= 1) {
      Span* prev_span = gc->page_span_map[page_index - 1];
      if (prev_span && prev_span->is_free) {
        is_merged = true;

        size_t merged_num_pages = prev_span->num_pages + span->num_pages;
        size_t merged_index = gc_size_to_index(merged_num_pages << LG_PAGE_SIZE);
        Bin* merged_bin = &gc->bins[merged_index];
        Span* merged_span = gc_alloc_span(gc, prev_span->ptr, merged_bin->free, merged_num_pages, merged_index, true);
        merged_bin->free = merged_span;
        // Clear middle mappings. That is the end of the previous and the beginning of the current.
        gc->page_span_map[page_index - 1] = NULL;
        gc->page_span_map[page_index] = NULL;
        gc_dalloc_span(gc, span);
        // Update span for the next check
        span = merged_span;
      }
    }

    if (next_page_index < gc->page_count - 1) {
      Span* next_span = gc->page_span_map[next_page_index];
      if (next_span && next_span->is_free) {
        is_merged = true;
        size_t merged_num_pages = span->num_pages + next_span->num_pages;
        size_t merged_index = gc_size_to_index(merged_num_pages << LG_PAGE_SIZE);
        Bin* merged_bin = &gc->bins[merged_index];
        merged_bin->free = gc_alloc_span(gc, span->ptr, merged_bin->free, merged_num_pages, merged_index, true);
        // Clear middle mappings. That is the end of the current and the beginning of the next.
        gc->page_span_map[next_page_index - 1] = NULL;
        gc->page_span_map[next_page_index] = NULL;
        gc_dalloc_span(gc, span);
      }
    }

    if (!is_merged) {
      span->is_free = true;
      span->next = bin->free;
      bin->free = span;
    }
  }
}

bool gc_init(GC* gc, size_t heap_size) {
  size_t page_count = (heap_size - gc_page_all_metadata_size(heap_size / PAGE_SIZE)) / PAGE_SIZE;
  size_t metadata_size = gc_page_all_metadata_size(page_count);

  // TODO: Minimum heap size

  gc->mem = mmap(NULL, heap_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
  if (gc->mem == NULL) {
    return false;
  }
  gc->page_span_map = (Span**)gc->mem;
  gc->spans = (Span*)((uintptr_t)gc->mem + gc_page_span_map_metadata_size(page_count));
  gc->pages = (void*)(round_to_alignment((uintptr_t)gc->mem + metadata_size + PAGE_SIZE / 2, PAGE_SIZE));
  gc->page_count = page_count;

  gc->free_spans = gc->spans;
  gc->free_spans[page_count - 1].next = NULL;
  for (size_t i = 0; i < page_count - 1; ++i) {
    gc->free_spans[i].next = &gc->free_spans[i + 1];
  }

  gc_calc_size_classes(gc);

  uintptr_t available = (uintptr_t)gc->pages - (uintptr_t)gc->mem;

  size_t index = gc_size_to_index(available);
  Bin* bin = &gc->bins[index];
  bin->free = gc_alloc_span(gc, gc->pages, bin->free, available >> LG_PAGE_SIZE, index, true);

  printf("%zu %f\n", available, (double)available / heap_size);
  printf("num spans lost to metadata %zu of %zu\n", metadata_size / PAGE_SIZE, heap_size / PAGE_SIZE);

  return true;
}

static size_t gc_size_to_index(size_t size) {
  assert(size > 0 && "Size cannot be zero");

  // Each group doubles so this rounds up over the size of the group
  // ((size * 2) - 1) then takes the floor of that.
  size_t group_index_max = (size_t)(__builtin_clzll((size << 1) - 1) ^ 63);

  size_t group_index = group_index_max < LG_NUM_GROUPS + LG_MIN_SIZE
                       ? 0
                       : group_index_max - (LG_NUM_GROUPS + LG_MIN_SIZE);

  size_t lg_delta = (group_index_max < LG_NUM_GROUPS + LG_MIN_SIZE + 1)
                    ? LG_MIN_SIZE
                    : group_index_max - LG_NUM_GROUPS - 1;

  size_t mask = ZU(-1) << lg_delta;
  size_t group_mask = (ZU(1) << LG_NUM_GROUPS) - 1;
  size_t index_in_group = (((size - 1) & mask) >> lg_delta) & group_mask;

  return (group_index << LG_NUM_GROUPS) + index_in_group;
}

static Span* gc_find_free_span(GC* gc, size_t num_pages) {
  Span* span = NULL;
  size_t first_index =  gc_size_to_index(num_pages * PAGE_SIZE);
  for (size_t i = first_index; i < NUM_SIZE_CLASSES; ++i) {
    Bin* bin = &gc->bins[i];
    if (!bin->is_slab && bin->free && bin->free->num_pages >= num_pages) {
      span = bin->free;
      bin->free = bin->free->next;
      break;
    }
  }
  return span;
}

static Span* gc_split_free_span(GC* gc, Span* span, size_t dest_index, Bin* dest_bin) {
  if (span->num_pages > dest_bin->num_pages) {
    Span* dest_span = gc_alloc_span(gc, span->ptr, NULL, dest_bin->num_pages, dest_index, false);

    if (dest_bin->is_slab) {
      dest_span->next = dest_bin->free;
      dest_bin->free = dest_span;
    } else {
      dest_span->next = dest_bin->full;
      dest_bin->full = dest_span;
    }

    size_t remaining_pages = span->num_pages - dest_bin->num_pages;
    size_t index = gc_size_to_index(remaining_pages << LG_PAGE_SIZE);
    Bin* bin = &gc->bins[index];
    void* ptr = (void*)((uintptr_t)span->ptr + (dest_bin->num_pages << LG_PAGE_SIZE));
    bin->free = gc_alloc_span(gc, ptr,  bin->free, remaining_pages, index, true);
    gc_dalloc_span(gc, span);

    return dest_span;
  }
  return span;
}

void* gc_alloc(GC* gc, size_t size) {
  size_t index = gc_size_to_index(size);
  Bin* bin = &gc->bins[index];
  Span* span = bin->free;

  if (span == NULL) {
    span = gc_find_free_span(gc, bin->num_pages);
    if (span == NULL) {
      // No free span. On no!
      return NULL;
    }
    span = gc_split_free_span(gc, span, index, bin);
  }

  if (bin->is_slab) {
    // TODO: Find first empty entry in slab
    return NULL;
  } else {
    return span->ptr;
  }
}

char* to_binary(size_t value, char* buf) {
  for (int b = 0; b < 64; ++b) {
      buf[b] = ((value >> (63 - b)) & 0x1) ? '1' : '0';
  }
  buf[64] = '\0';
  return buf;
}

void print_binary(size_t value) {
  char buf[65];
  printf("%s\n", to_binary(value, buf));
}

int main() {
  GC gc;
  gc_init(&gc, 160 * 1024 * 1024);
  void* m1 = gc_alloc(&gc, 4096);
  void* m2 = gc_alloc(&gc, 4096);
  gc_dalloc(&gc, m1, 4096);
  gc_dalloc(&gc, m2, 4096);
  //m = gc_alloc(&gc, 4096);
}
