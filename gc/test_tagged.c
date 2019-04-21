#include "unity.h"

#include "gc.h"

void test_tagged_integer_from_and_to() {
  GCTagged tagged;

  tagged = gc_tagged_from_integer(0);
  TEST_ASSERT_EQUAL_INT64(0, gc_tagged_to_integer(tagged));

  tagged = gc_tagged_from_integer(1);
  TEST_ASSERT_EQUAL_INT64(1, gc_tagged_to_integer(tagged));

  tagged = gc_tagged_from_integer(-1);
  TEST_ASSERT_EQUAL_INT64(-1, gc_tagged_to_integer(tagged));

  tagged = gc_tagged_from_integer(4611686018427387903L);
  TEST_ASSERT_EQUAL_INT64(4611686018427387903L, gc_tagged_to_integer(tagged));

  tagged = gc_tagged_from_integer(-4611686018427387904L);
  TEST_ASSERT_EQUAL_INT64(-4611686018427387904L, gc_tagged_to_integer(tagged));
}

int main() {
  UNITY_BEGIN();
  RUN_TEST(test_tagged_integer_from_and_to);
  UNITY_END();
}
