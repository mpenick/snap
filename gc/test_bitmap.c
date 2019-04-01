#include "unity.h"

#include "gc.h"

#define BITMAP_SIZE 3

void test_bitmap_partial_word() {
  Bitmap bitmap[BITMAP_SIZE];
  for (size_t b = 1; b <= BITMAP_SIZE * BITMAP_WORD_NUM_BITS; ++b) {
    bitmap_init(bitmap, b);
    for (size_t i = 0; i < b; ++i) {
      bitmap_set(bitmap, i);
      TEST_ASSERT_TRUE(bitmap_is_set(bitmap, i));
    }
    TEST_ASSERT_TRUE(bitmap_is_full(bitmap, b));
  }
}

void test_bitmap_set() {
  Bitmap bitmap[BITMAP_SIZE];
  bitmap_init(bitmap, BITMAP_SIZE * BITMAP_WORD_NUM_BITS);

  for (size_t i = 0; i < BITMAP_SIZE * BITMAP_WORD_NUM_BITS; ++i) {
    TEST_ASSERT_FALSE(bitmap_is_set(bitmap, i));
    bitmap_set(bitmap, i);
    TEST_ASSERT_TRUE(bitmap_is_set(bitmap, i));
    bitmap_unset(bitmap, i);
    TEST_ASSERT_FALSE(bitmap_is_set(bitmap, i));
  }
}

void test_bitmap_sfu() {
  Bitmap bitmap[BITMAP_SIZE];
  bitmap_init(bitmap, BITMAP_SIZE * BITMAP_WORD_NUM_BITS);

  TEST_ASSERT_FALSE(bitmap_is_full(bitmap, BITMAP_SIZE));
  for (size_t i = 0; i < BITMAP_SIZE * BITMAP_WORD_NUM_BITS; ++i) {
    size_t b = bitmap_sfu(bitmap);
    TEST_ASSERT_EQUAL_UINT(i, b);
  }
  TEST_ASSERT_TRUE(bitmap_is_full(bitmap, BITMAP_SIZE));
}

int main() {
  UNITY_BEGIN();
  RUN_TEST(test_bitmap_partial_word);
  RUN_TEST(test_bitmap_set);
  RUN_TEST(test_bitmap_sfu);
  UNITY_END();
}
