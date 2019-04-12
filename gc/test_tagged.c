#include "unity.h"

#include "gc.h"

void test_tagged_integer_values() {
  GCTagged tagged;

  //smint
  int64_t MAX_SMINT_VALUE = (1L << 62);

  //tagged = gc_tagged_from_integer(1);
  //TEST_ASSERT_EQUAL_INT64(1, gc_tagged_to_integer(tagged));

  //tagged = gc_tagged_from_integer(-1);
  //TEST_ASSERT_EQUAL_INT64(-1, gc_tagged_to_integer(tagged));
}


int main() {
  UNITY_BEGIN();
  RUN_TEST(test_tagged_integer_values);
  UNITY_END();
}
