#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef union SnapValue_ SnapValue;

union SnapValue_ {
  double f;
  union {
    void* p;
    struct {
#ifdef SNAP_BIG_ENDIAN
    uint32_t type;
    int32_t i;
#else
    int32_t i;
    uint32_t type;
#endif
    };
  };
};

enum {
  SNAP_TYPE_FALSE = 1,
  SNAP_TYPE_TRUE,
  SNAP_TYPE_NIL,
  SNAP_TYPE_FLOAT,
  SNAP_TYPE_INT,
  SNAP_TYPE_STR,
  SNAP_TYPE_CELL,
  SNAP_TYPE_ARRAY,
  SNAP_TYPE_LIST,
  SNAP_TYPE_HASH,
  SNAP_TYPE_PTR
};

#define snap_type(v) ((v).type > 0xfff00000 ? (((v).type >> 16) & 0x0f) : SNAP_TYPE_FLOAT)
#define snap_mktype(t) (0xfff00000 | (((uint32_t)(t)) << 16))

#define snap_float(v) ((v).f)
#define snap_int(v) ((int32_t)v.i)
#define snap_ptr(v) ((void*)(((uintptr_t)(v).p) & 0x0000ffffffffffff))


static inline SnapValue snap_int_value(int32_t i) {
  SnapValue v;
  v.type = snap_mktype(SNAP_TYPE_INT);
  v.i = i;
  return v;
}

static inline SnapValue snap_float_value(double f) {
  SnapValue v;
  if (f != f) {
    v.type = 0x7ff80000;
    v.i = 0;
  } else {
    v.f = f;
  }
  return v;
}

static inline SnapValue snap_object_value(void* p) {
  SnapValue v;
  v.type = snap_mktype(SNAP_TYPE_STR);
  v.i = 0;
  v.p = (void*)((uintptr_t)p | (uintptr_t)v.p);
  return v;
}

int main() {
  void* p = malloc(sizeof(SnapValue));
  SnapValue v = snap_object_value(p);
  printf("%d %p (%p)\n", snap_type(v), snap_ptr(v), p);

  //v = snap_float_value(99.9999);
  v = snap_float_value(nan(0));
  printf("%d %f\n", snap_type(v), snap_float(v));

  v = snap_int_value(99);
  printf("%d %d\n", snap_type(v), snap_int(v));
  return 0;
}
