#include "unity.h"

#include "gc.h"

#define GC_HEAP_SIZE (16 * 1024 * 1024)

size_t count_spans(GC* gc) {
  size_t count = 1;

  Span* curr = gc->span_map[0];
  TEST_ASSERT_NOT_NULL(curr);

  for (size_t i = 1; i < gc->page_count; ++i) {
    Span* span = gc->span_map[i];
    if (span && curr != span) {
      curr = span;
      count++;
    }
  }

  return count;
}

void test_alloc_full_pages() {
  GC gc;
  gc_init(&gc, GC_HEAP_SIZE);
  TEST_ASSERT_EQUAL_INT(1, count_spans(&gc));

  void* m1 = gc_alloc(&gc, 4096);
  TEST_ASSERT_EQUAL_INT(2, count_spans(&gc));

  void* m2 = gc_alloc(&gc, 4096);
  TEST_ASSERT_EQUAL_INT(3, count_spans(&gc));

  gc_dalloc(&gc, m1);
  gc_dalloc(&gc, m2);
  TEST_ASSERT_EQUAL_INT(1, count_spans(&gc));

  void* m3 = gc_alloc(&gc, 4096);
  TEST_ASSERT_EQUAL_INT(2, count_spans(&gc));

  gc_dalloc(&gc, m3);
  TEST_ASSERT_EQUAL_INT(1, count_spans(&gc));
}

void test_alloc_slab_pages() {
  GC gc;
  gc_init(&gc, GC_HEAP_SIZE);

  void* m[4];
  for (size_t i = 0; i < 4; ++i) {
    m[i] = gc_alloc(&gc, 1024);
  }
  TEST_ASSERT_EQUAL_INT(2, count_spans(&gc));

  for (size_t i = 0; i < 4; ++i) {
    gc_dalloc(&gc, m[i]);
  }
  TEST_ASSERT_EQUAL_INT(1, count_spans(&gc));
}

void test_alloc_multiple_page_slab() {
  GC gc;
  gc_init(&gc, GC_HEAP_SIZE);

  // Fill up the first page
  void* m[PAGE_SIZE / 48];
  for (size_t i = 0; i < PAGE_SIZE / 48; ++i) {
    m[i] = gc_alloc(&gc, 48);
  }
  TEST_ASSERT_EQUAL_INT(2, count_spans(&gc));

  void* n = gc_alloc(&gc, 48); // Allocate on the middle page
  TEST_ASSERT_EQUAL_INT(2, count_spans(&gc));

  gc_dalloc(&gc, n); // Deallocate on the middle page
  TEST_ASSERT_EQUAL_INT(2, count_spans(&gc));

  for (size_t i = 0; i < PAGE_SIZE / 48; ++i) {
    gc_dalloc(&gc, m[i]);
  }
  TEST_ASSERT_EQUAL_INT(1, count_spans(&gc));
}

void test_alloc_multiples_of_page_size() {
  GC gc;
  gc_init(&gc, GC_HEAP_SIZE);

  for (size_t i = 1; i <= 16; ++i) {
    void* m = gc_alloc(&gc, i * PAGE_SIZE);
    TEST_ASSERT_EQUAL_INT(2, count_spans(&gc));
    gc_dalloc(&gc, m);
    TEST_ASSERT_EQUAL_INT(1, count_spans(&gc));
  }
}

int main() {
  UNITY_BEGIN();
  RUN_TEST(test_alloc_full_pages);
  RUN_TEST(test_alloc_slab_pages);
  RUN_TEST(test_alloc_multiple_page_slab);
  RUN_TEST(test_alloc_multiples_of_page_size);
  UNITY_END();
}
