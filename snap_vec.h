#ifndef SNAP_VEC_H
#define SNAP_VEC_H

#include "snap_val.h"

#include <stdbool.h>

typedef struct {
  SValue* items;
  int capacity;
  int size;
} SnapVec;

void snap_vec_init(SnapVec* vec);
void snap_vec_destroy(SnapVec* vec);
void snap_vec_push(SnapVec* vec, SValue val);
bool snap_vec_pop(SnapVec* vec, SValue* val);

#endif

