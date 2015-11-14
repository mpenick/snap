#include "snap_vec.h"

#include <stdlib.h>

#define VEC_MIN_CAPACITY 2

void snap_vec_init(SnapVec* vec) {
  vec->items = (SValue*)malloc(VEC_MIN_CAPACITY * sizeof(SValue));
  vec->capacity = VEC_MIN_CAPACITY;
  vec->size = 0;
}

void snap_vec_destroy(SnapVec* vec) {
  free((void*)vec->items);
}

void snap_vec_push(SnapVec* vec, SValue val) {
  if (vec->size >= vec->capacity) {
    int new_capacity = vec->capacity < 4096 ? vec->capacity * 4 : vec->capacity * 2;
    vec->items = (SValue*)realloc(vec->items, new_capacity * sizeof(SValue));
    vec->capacity = new_capacity;
  }
  vec->items[vec->size++] = val;
}

bool snap_vec_pop(SnapVec* vec, SValue* val) {
  if (vec->size > 0) {
    *val = vec->items[--vec->size];
    return true;
  }
  return false;
}
