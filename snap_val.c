#include "snap_val.h"

#include "snap.h"

#include <limits.h>
#include <math.h>
#include <string.h>

#define snap_compare_(a, b) ((a) < (b) ? -1 : (a) > (b))

static uint32_t calc_hash(const void* data, size_t length) {
  uint32_t h = 0x811c9dc5;
  uint8_t* p = (uint8_t* )data;
  uint8_t* e = (uint8_t* )data + length;
  while (p < e) {
    h ^= (uint32_t)*p++;
    h *= 0x01000193;
  }
  return h;
}

static int hash_float(double n) {
  int i;
  unsigned int u;
  if (!isfinite(n)) return 0;
  n = frexp(n, &i) * -(double)INT_MIN;
  u = (unsigned int)n + (unsigned int)i;
  return (int)(u <= (unsigned int)INT_MAX ? u : ~u);
}

static int hash_string(SSymStr* s) {
  return (int)calc_hash((void*)s->data, s->len);
}

static int compare_string(SSymStr* s1, SSymStr* s2) {
  if (s1->len != s1->len) {
    return s1->len < s2->len ? -1 : 1;
  }
  return strncmp(s1->data, s2->data, s1->len);
}

static int hash_pointer(void* ptr) {
  uintptr_t p = (uintptr_t)ptr;
  return (int)((p >> 4) | (p << (8 * sizeof(ptr) - 4)));
}

int snap_hash(SValue val) {
  switch (val.type) {
    case STYPE_NIL:
      return 0;
    case STYPE_BOOL:
      return val.b ? 1 : 0;
    case STYPE_INT:
      return val.i;
    case STYPE_FLOAT:
      return hash_float(val.f);
    case STYPE_FORM:
      return val.i;
    case STYPE_CFUNC:
      return hash_pointer((void*)val.c);
    case STYPE_SYM:
    case STYPE_STR:
      return hash_string((SSymStr*)val.o);
    case STYPE_ERR:
    case STYPE_CONS:
    case STYPE_HASH:
    case STYPE_SCOPE:
    case STYPE_FN:
      return hash_pointer((void*)val.o);
  }
  return 0;
}

int snap_compare(SValue val1, SValue val2) {
  if (val1.type != val2.type) {
    return val1.type < val2.type ? -1 : 1;
  }

  switch (val1.type) {
    case STYPE_NIL:
      return 0;
    case STYPE_BOOL:
      return snap_compare_(val1.b, val2.b);
    case STYPE_INT:
      return snap_compare_(val1.i, val2.i);
    case STYPE_FLOAT:
      return snap_compare_(val1.f, val2.f);
    case STYPE_FORM:
      return snap_compare_(val1.i, val2.i);
    case STYPE_CFUNC:
      return snap_compare_((void*)val1.c, (void*)val2.c);
    case STYPE_SYM:
    case STYPE_STR:
      return compare_string((SSymStr*)val1.o, (SSymStr*)val2.o);
    case STYPE_ERR:
    case STYPE_CONS:
    case STYPE_HASH:
    case STYPE_SCOPE:
    case STYPE_FN:
      return snap_compare_(val1.o, val2.o);
  }
  return 0;
}
