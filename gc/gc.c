#include "gc.h"

#include <stdio.h>
#include <string.h>

#include <sys/mman.h>

static inline uintptr_t round_to_alignment(uintptr_t value, uintptr_t alignment) {
   return (value + (alignment - 1)) & ~(alignment - 1);
}

size_t bitmap_alloc_size(size_t num_bits) {
  return round_to_alignment(num_bits,
                            BITMAP_WORD_NUM_BITS) / BITMAP_WORD_NUM_BITS;
}

void bitmap_init(Bitmap* bitmap, size_t num_bits) {
  size_t num_words = (num_bits + (BITMAP_WORD_NUM_BITS - 1)) >> LG_BITMAP_WORD_NUM_BITS;
  memset(bitmap, 0xFF, num_words * sizeof(Bitmap));
  size_t remaining = num_words * BITMAP_WORD_NUM_BITS - num_bits;
  if (remaining > 0) {
    Bitmap left_over = (ZU(1) << (BITMAP_WORD_NUM_BITS - remaining)) - 1;
    bitmap[num_words - 1] = left_over;
  }
}

static size_t gc_page_span_map_metadata_size(size_t page_count) {
  return page_count * sizeof(GCSpan*);
}

static size_t gc_page_all_metadata_size(size_t page_count) {
  return gc_page_span_map_metadata_size(page_count) + page_count * sizeof(GCSpan);
}

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

static void gc_bin_init(GCBin* bin, bool is_slab, size_t num_pages, size_t size) {
  bin->is_slab = is_slab;
  bin->num_pages = num_pages;
  bin->size = size;
  size_t num_slab_entries = (num_pages * PAGE_SIZE) / size;
  assert(num_slab_entries < UINT16_MAX && "Invalid number of slab entries");
  bin->num_slab_entries = (uint16_t)num_slab_entries;
  mlist_init(&bin->free);
  mlist_init(&bin->full);
}

static void gc_calc_size_classes(GC* gc) {
  size_t lg_base = LG_MIN_SIZE;
  size_t lg_delta = LG_QUANTUM;
  size_t index = 0;

  for (size_t n = 0; n < 2 * NUM_GROUPS; ++n) {
    size_t size = ((size_t)1 << lg_base) + (n << lg_delta);
    size_t num_pages = gc_calc_num_pages(size);
    gc_bin_init(&gc->bins[index], true, num_pages, size);
    index++;
  }

  lg_base+=2;

  while (((size_t)1 << lg_base) + (NUM_GROUPS << lg_delta) < ((size_t)1 << LG_VADDR_SIZE) * PAGE_SIZE) {
    lg_base++;
    lg_delta++;
    for (size_t n = 1; n <= NUM_GROUPS; ++n) {
      size_t size = ((size_t)1 << lg_base) + (n << lg_delta);

      if (size < 4 * PAGE_SIZE) {
        size_t num_pages = gc_calc_num_pages(size);
        bool is_slab = size < PAGE_SIZE || size % PAGE_SIZE != 0;
        gc_bin_init(&gc->bins[index], is_slab, num_pages, size);
      } else {
        gc_bin_init(&gc->bins[index], false, size >> LG_PAGE_SIZE, size);
      }
      index++;
    }
  }
  assert(index == NUM_SIZE_CLASSES && "Calculated an invalid number of size classes");
}

static uint8_t gc_size_to_index(size_t size);

static size_t gc_ptr_to_bit_index(GCSpan* span, GCBin* bin, void* ptr) {
  size_t bit_index = ((uintptr_t)ptr - (uintptr_t)span->ptr) / bin->size;
  assert(span->slab_entry_count > 0 && "Object not allocated in the span");
  assert(bit_index < bin->num_slab_entries && "Invalid slab entry");
  return bit_index;
}

static GCBin* gc_span_to_bin(GC* gc, GCSpan* span) {
  assert(span && span->size_index < NUM_SIZE_CLASSES && "Invalid span");
  return &gc->bins[span->size_index];
}

static size_t gc_ptr_to_page_index(GC* gc, void* ptr) {
  assert((uintptr_t)ptr >= (uintptr_t)gc->pages &&
         (uintptr_t)ptr < (uintptr_t)gc->pages + PAGE_SIZE * gc->page_count &&
         "Pointer is not own by this allocator");
  uintptr_t page_ptr = (uintptr_t)ptr& (ZU(-1) << LG_PAGE_SIZE);
  size_t page_index = (page_ptr - (uintptr_t)gc->pages) >> LG_PAGE_SIZE;
  return page_index;
}

static GCSpan* gc_page_index_to_span(GC* gc, size_t page_index) {
  GCSpan* span = gc->span_map[page_index];
  GCSpan* orig = span;
  while (span == NULL && page_index > 0) {
    span = gc->span_map[--page_index];
  }
  assert((span != NULL || page_index > 0) &&
         "Span mapping doesn't have a proper lower bound");
  (void)orig;
  assert((orig != NULL || gc_span_to_bin(gc, orig)->is_slab) &&
         "Page indexes that point to the middle of spans should belong to a slab");
  return span;
}

static void gc_mark_span_map(GC*gc, GCSpan* span) {
  size_t page_index = gc_ptr_to_page_index(gc, span->ptr);
  // Mark first and last pages in the mapping
  gc->span_map[page_index] = span;
  gc->span_map[page_index + span->num_pages - 1] = span;
}

static GCSpan* gc_make_span(GC* gc, void* ptr, uint8_t size_index, size_t num_pages) {
  assert(!mlist_is_empty(&gc->free_spans) && "A span should always be available");

  GCSpan* span = mlist_entry(GCSpan, gc->free_spans.next, list);
  mlist_pop_front(&gc->free_spans);

  span->ptr = ptr;
  span->num_pages = num_pages;
  span->size_index = size_index;
  span->is_free = true;
  mlist_prepend(&gc->bins[size_index].free, &span->list);
  gc_mark_span_map(gc, span);

  return span;
}

static void gc_free_span(GC* gc, GCSpan* span) {
  mlist_prepend(&gc->free_spans, &span->list);
}

bool gc_init(GC* gc, size_t heap_size) {
  if (heap_size < 2 * PAGE_SIZE) {
    return false;
  }

  size_t page_count = (heap_size - gc_page_all_metadata_size(heap_size / PAGE_SIZE)) / PAGE_SIZE;
  size_t metadata_size = gc_page_all_metadata_size(page_count);

  gc->mem = mmap(NULL, heap_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
  if (gc->mem == NULL) {
    return false;
  }
  gc->span_map = (GCSpan**)gc->mem;
  gc->spans = (GCSpan*)((uintptr_t)gc->mem + gc_page_span_map_metadata_size(page_count));
  gc->pages = (void*)((uintptr_t)gc->mem + round_to_alignment(metadata_size + PAGE_SIZE / 2, PAGE_SIZE));
  gc->page_count = page_count;

  mlist_init(&gc->free_spans);
  for (size_t i = 0; i < page_count; ++i) {
    mlist_prepend(&gc->free_spans, &gc->spans[i].list);
  }

  gc_calc_size_classes(gc);

  // TODO: Assert/Verify these numbers
  uintptr_t available =  heap_size - ((uintptr_t)gc->pages - (uintptr_t)gc->mem);

  uint8_t index = gc_size_to_index(available);
  gc_make_span(gc, gc->pages, index, available >> LG_PAGE_SIZE);

  fprintf(stderr, "%f%% lost to metadata\n", (1.0 - (double)available / heap_size) * 100.0);
  fprintf(stderr, "num spans lost to metadata %zu of %zu\n", metadata_size / PAGE_SIZE, heap_size / PAGE_SIZE);

  return true;
}

static uint8_t gc_size_to_index(size_t size) {
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

  size_t index = (group_index << LG_NUM_GROUPS) + index_in_group;
  assert(index < UINT8_MAX && "Invalid size index calculated");
  return (uint8_t)index;
}

static GCSpan* gc_alloc_span(GC* gc, size_t num_pages) {
  size_t first_index =  gc_size_to_index(num_pages * PAGE_SIZE);
  // TODO: This could be faster if it used FFS in a bitmap like TLSF
  for (size_t i = first_index; i < NUM_SIZE_CLASSES; ++i) {
    GCBin* bin = &gc->bins[i];
    if (!bin->is_slab && !mlist_is_empty(&bin->free)) {
      GCSpan* span = mlist_entry(GCSpan, bin->free.next, list);
      if (span->num_pages >= num_pages) {
        mlist_pop_front(&bin->free);
        return span;
      }
    }
  }
  return NULL;
}

static void gc_split_span(GC* gc, GCSpan* span, uint8_t dest_index, GCBin* dest_bin) {
  if (span->num_pages > dest_bin->num_pages) {
    size_t remaining_pages = span->num_pages - dest_bin->num_pages;

    // Use the same ptr because it's the beginning of the span
    span->num_pages = dest_bin->num_pages;
    span->size_index = dest_index;
    span->is_free = false;
    mlist_prepend(dest_bin->is_slab ? &dest_bin->free : &dest_bin->full, &span->list);
    gc_mark_span_map(gc, span);

    uint8_t index = gc_size_to_index(remaining_pages << LG_PAGE_SIZE);
    void* ptr = (void*)((uintptr_t)span->ptr + (dest_bin->num_pages << LG_PAGE_SIZE));
    gc_make_span(gc, ptr, index, remaining_pages);
  }
}

void* gc_alloc(GC* gc, size_t size) {
  uint8_t index = gc_size_to_index(size);
  GCBin* bin = &gc->bins[index];
  GCSpan* span = NULL;

  if (!mlist_is_empty(&bin->free)) {
    span = mlist_entry(GCSpan, bin->free.next, list);
  } else {
    span = gc_alloc_span(gc, bin->num_pages);
    if (span == NULL) {
      // No free span. On no!
      return NULL;
    }
    gc_split_span(gc, span, index, bin);
    if (bin->is_slab) {
      bitmap_init(span->small_alloced, bin->num_slab_entries);
    }
  }

  if (bin->is_slab) {
    size_t bit_index = bitmap_sfu(span->small_alloced);
    if (++span->slab_entry_count == bin->num_slab_entries) {
      mlist_remove(&span->list);
      mlist_prepend(&bin->full, &span->list);
    }
    return (void*)((uintptr_t)span->ptr + bit_index * bin->size);
  } else {
    return span->ptr;
  }
}

GCObject* gc_object_new(GC* gc, size_t size) {
  assert(size > sizeof(GCObject) && "Invalid object size");
  GCObject* obj = gc_alloc(gc, size);
  obj->header = 0;
  return obj;
}

GCContainer* gc_container_new(GC* gc, size_t num_entries) {
  assert(num_entries > 0 && "Invalid number of entries for container");
  GCContainer* cont = gc_alloc(gc, sizeof(GCObject) + num_entries * sizeof(GCTagged));
  cont->base.header = gc_header_set_is_container(0, true);
  cont->base.header = gc_header_set_num_entries(cont->base.header, num_entries);
  return cont;
}

static void gc_coalesce_span(GC* gc, GCSpan* span, void* coalesced_ptr, size_t coalesced_num_pages, size_t page_index) {
  uint8_t coalesced_index = gc_size_to_index(coalesced_num_pages << LG_PAGE_SIZE);
  GCBin* coalesced_bin = &gc->bins[coalesced_index];

  assert(!coalesced_bin->is_slab && "Attemted to add coalesced span to slab bin");

  span->ptr = coalesced_ptr;
  span->num_pages =  coalesced_num_pages;
  span->size_index = coalesced_index;
  mlist_prepend(&coalesced_bin->free, &span->list);

  // Clear middle mappings. These will need to be fixed later in cases where
  // either span only has a single page.
  gc->span_map[page_index] = NULL;
  gc->span_map[page_index - 1] = NULL;
}

static void gc_dalloc_span(GC* gc, GCSpan* span, size_t page_index) {
  uintptr_t next_page_index = page_index + span->num_pages;

  mlist_remove(&span->list); // Remove from current list
  span->is_free = true;

  bool is_coalesced = false;

  if (page_index >= 1) {
    GCSpan* prev_span = gc->span_map[page_index - 1];
    if (prev_span && prev_span->is_free) {
      is_coalesced = true;
      mlist_remove(&prev_span->list); // Remove from free list
      gc_coalesce_span(gc, span, prev_span->ptr, prev_span->num_pages + span->num_pages, page_index);
      gc_free_span(gc, prev_span);
    }
  }

  if (next_page_index < gc->page_count - 1) {
    GCSpan* next_span = gc->span_map[next_page_index];
    if (next_span && next_span->is_free) {
      is_coalesced = true;
      mlist_remove(&next_span->list); // Remove from free list
      gc_coalesce_span(gc, span, span->ptr, span->num_pages + next_span->num_pages, next_page_index);
      gc_free_span(gc, next_span);
    }
  }

  if (is_coalesced) {
    gc_mark_span_map(gc, span); // Cleanup mappings
  } else {
    size_t index = gc_size_to_index(span->num_pages << LG_PAGE_SIZE);
    GCBin* bin = &gc->bins[index];
    assert(!bin->is_slab && "Attemted to add span to slab bin");
    mlist_prepend(&bin->free, &span->list);
  }
}

void gc_dalloc(GC* gc, void* ptr) {
  size_t page_index = gc_ptr_to_page_index(gc, ptr);
  GCSpan* span = gc_page_index_to_span(gc, page_index);
  GCBin* bin = gc_span_to_bin(gc, span);

  if (bin->is_slab) {
    size_t bit_index = gc_ptr_to_bit_index(span, bin, ptr);
    bool was_full = span->slab_entry_count == bin->num_slab_entries;
    bitmap_unset(span->small_alloced, bit_index);
    if (--span->slab_entry_count == 0) {
      gc_dalloc_span(gc, span, page_index);
    } else if (was_full) {
      mlist_remove(&span->list);
      mlist_prepend(&bin->free, &span->list);
    }
  } else {
    gc_dalloc_span(gc, span, page_index);
  }
}

void gc_collect(GC* gc) {
}

static void gc_mark_ptr(GC* gc, void* ptr) {
  size_t page_index = gc_ptr_to_page_index(gc, ptr);
  GCSpan* span = gc_page_index_to_span(gc, page_index);
  GCBin* bin = gc_span_to_bin(gc, span);
  if (bin->is_slab) {
    size_t bit_index = gc_ptr_to_bit_index(span, bin, ptr);
    bitmap_set(span->small_marked, bit_index);
  } else {
    span->large_marked = true;
  }
}

static void gc_mark_object(GC* gc, GCObject* obj) {
  if (gc_header_is_container(obj->header)) {
    if (!gc_header_is_gray(obj->header)) {
      obj->header = gc_header_set_is_gray(obj->header, true);
      // Add to grey stack
    }
  } else {
    gc_mark_ptr(gc, obj);
  }
}

static void gc_mark_entries(GC* gc, GCContainer* cont) {
  const size_t num_entries = gc_header_num_entries(cont->base.header);
  for (size_t i = 0; i < num_entries; ++i) {
    GCTagged entry = cont->entries[i];
    if (gc_tagged_is_object(entry)) {
      gc_mark_object(gc, gc_tagged_to_object(entry));
    }
  }
  gc_mark_ptr(gc, cont);
}
