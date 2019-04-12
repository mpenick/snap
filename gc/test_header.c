#include "unity.h"

#include "gc.h"

void test_header_is_gray() {
  GCHeader header = 0;
  TEST_ASSERT_FALSE(gc_header_is_gray(header));
  header = gc_header_set_is_gray(header, true);
  TEST_ASSERT_TRUE(gc_header_is_gray(header));
  header = gc_header_set_is_gray(header, false);
  TEST_ASSERT_FALSE(gc_header_is_gray(header));
}

void test_header_is_container() {
  GCHeader header = 0;
  TEST_ASSERT_FALSE(gc_header_is_container(header));
  header = gc_header_set_is_container(header, true);
  TEST_ASSERT_TRUE(gc_header_is_container(header));
  header = gc_header_set_is_container(header, false);
  TEST_ASSERT_FALSE(gc_header_is_container(header));
}

void test_header_type() {
  GCHeader header = 0;
  TEST_ASSERT_EQUAL_INT16(gc_header_type(header), 0);
  header = gc_header_set_type(header, 0xFFFF);
  TEST_ASSERT_EQUAL_UINT16(gc_header_type(header), 0xFFFF);
  header = gc_header_set_type(header, 2);
  TEST_ASSERT_EQUAL_INT16(gc_header_type(header), 2);
  header = gc_header_set_type(header, 0);
  TEST_ASSERT_EQUAL_INT16(gc_header_type(header), 0);
}

void test_header_num_entries() {
  GCHeader header = 0;
  TEST_ASSERT_EQUAL_INT16(gc_header_num_entries(header), 0);
  header = gc_header_set_num_entries(header, 0x3FFFFFFFFFFFL);
  TEST_ASSERT_EQUAL_UINT16(gc_header_num_entries(header), 0x3FFFFFFFFFFFL);
  header = gc_header_set_num_entries(header, 2);
  TEST_ASSERT_EQUAL_INT16(gc_header_num_entries(header), 2);
  header = gc_header_set_num_entries(header, 0);
  TEST_ASSERT_EQUAL_INT16(gc_header_num_entries(header), 0);
}

void test_header_all() {
  GCHeader header = 0;
  TEST_ASSERT_FALSE(gc_header_is_gray(header));
  TEST_ASSERT_FALSE(gc_header_is_container(header));
  TEST_ASSERT_EQUAL_INT16(gc_header_type(header), 0);
  TEST_ASSERT_EQUAL_INT16(gc_header_num_entries(header), 0);

  header = gc_header_set_is_gray(header, true);
  header = gc_header_set_is_container(header, true);
  header = gc_header_set_type(header, 0xFFFF);
  header = gc_header_set_num_entries(header, 0x3FFFFFFFFFFFL);

  TEST_ASSERT_TRUE(gc_header_is_gray(header));
  TEST_ASSERT_TRUE(gc_header_is_container(header));
  TEST_ASSERT_EQUAL_UINT16(gc_header_type(header), 0xFFFF);
  TEST_ASSERT_EQUAL_UINT16(gc_header_num_entries(header), 0x3FFFFFFFFFFFL);

  header = gc_header_set_is_gray(header, false);

  TEST_ASSERT_FALSE(gc_header_is_gray(header));
  TEST_ASSERT_TRUE(gc_header_is_container(header));
  TEST_ASSERT_EQUAL_UINT16(gc_header_type(header), 0xFFFF);
  TEST_ASSERT_EQUAL_UINT16(gc_header_num_entries(header), 0x3FFFFFFFFFFFL);

  header = gc_header_set_is_gray(header, true);
  header = gc_header_set_is_container(header, false);

  TEST_ASSERT_TRUE(gc_header_is_gray(header));
  TEST_ASSERT_FALSE(gc_header_is_container(header));
  TEST_ASSERT_EQUAL_UINT16(gc_header_type(header), 0xFFFF);
  TEST_ASSERT_EQUAL_UINT16(gc_header_num_entries(header), 0x3FFFFFFFFFFFL);

  header = gc_header_set_is_container(header, true);
  header = gc_header_set_type(header, 2);

  TEST_ASSERT_TRUE(gc_header_is_gray(header));
  TEST_ASSERT_TRUE(gc_header_is_container(header));
  TEST_ASSERT_EQUAL_UINT16(gc_header_type(header), 2);
  TEST_ASSERT_EQUAL_UINT16(gc_header_num_entries(header), 0x3FFFFFFFFFFFL);

  header = gc_header_set_type(header, 0xFFFF);
  header = gc_header_set_num_entries(header, 2);

  TEST_ASSERT_TRUE(gc_header_is_gray(header));
  TEST_ASSERT_TRUE(gc_header_is_container(header));
  TEST_ASSERT_EQUAL_UINT16(gc_header_type(header), 0xFFFF);
  TEST_ASSERT_EQUAL_UINT16(gc_header_num_entries(header), 2);
}

int main() {
  UNITY_BEGIN();
  RUN_TEST(test_header_is_gray);
  RUN_TEST(test_header_is_container);
  RUN_TEST(test_header_type);
  RUN_TEST(test_header_num_entries);
  RUN_TEST(test_header_all);
  UNITY_END();
}
