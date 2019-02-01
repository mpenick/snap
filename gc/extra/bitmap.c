#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __x86_64__
typedef unsigned long long Bitmap;
#define BITMAP_WORD_NUM_BITS 64
#define BITMAP_LOG_WORD_NUM_BITS 6
#else
typedef unsigned long Bitmap;
#define BITMAP_WORD_NUM_BITS 32
#define BITMAP_LG_WORD_NUM_BITS 5
#endif

#define BITMAP_WORD_NUM_BITS_MASK (BITMAP_WORD_NUM_BITS - 1)

size_t next_pow_2(size_t n) {
  size_t p = 2;
  while (p < n) p *= 2;
  return p;
}

static inline size_t round_to_alignment(size_t value, size_t alignment) {
   return (value + (alignment - 1)) & ~(alignment - 1);
}

size_t bitmap_alloc_size(size_t num_bits) {
  return round_to_alignment(num_bits,
                            BITMAP_WORD_NUM_BITS) / BITMAP_WORD_NUM_BITS;
}

void bitmap_init(Bitmap* bitmap, size_t num_bits) {
  memset(bitmap, 0, num_bits / 8);
}

void bitmap_set(Bitmap* bitmap, size_t bit) {
  size_t index = bit >> BITMAP_LOG_WORD_NUM_BITS;
  bitmap[index] ^= ((Bitmap)0x1) << (bit & BITMAP_WORD_NUM_BITS_MASK);
}

bool bitmap_is_set(Bitmap* bitmap, size_t bit) {
  size_t index = bit >> BITMAP_LOG_WORD_NUM_BITS;
  return bitmap[index] & ((Bitmap)0x1) << (bit & BITMAP_WORD_NUM_BITS_MASK);
}

size_t bitmap_ffs(Bitmap* bitmap) {
  size_t i = 0;
  size_t bit;
  while ((bit = (size_t)__builtin_ffsll(bitmap[i])) == 0) {
    i++;
  }
  return (i << BITMAP_LOG_WORD_NUM_BITS) + (bit - 1);
}

/*
 * B | M
 * -----
 * 0 | 0 | Extent
 * 0 | 1 | Free
 * 1 | 0 | White
 * 1 | 1 | Black
 * -----
 *
 * B: 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 * M: 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 */

void test(Bitmap* bm, size_t index) {
  printf("is_set(%zu) = %s\n", index, bitmap_is_set(bm, index) ? "true" : "false");
  bitmap_set(bm, index);
  printf("is_set(%zu) = %s\n", index, bitmap_is_set(bm, index) ? "true" : "false");
}

int main() {
#if 0
  test(bm, 129);
  test(bm, 65);
  long long l = 0x4;
  printf("ffs %d\n", __builtin_ffs(l));
#endif
  Bitmap* bm = (Bitmap*)malloc(bitmap_alloc_size(129));
  bitmap_init(bm, 129);
  test(bm, 65);
  printf("ffs %zu\n", bitmap_ffs(bm));
}
