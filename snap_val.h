#ifndef SNAP_VAL_H
#define SNAP_VAL_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef long SnapInt;
typedef double SnapFloat;

enum {
  STYPE_UNDEF    = 0,
  STYPE_NIL      = 1,
  STYPE_BOOL     = 2,
  STYPE_INT      = 3,
  STYPE_FLOAT    = 4,
  STYPE_FORM     = 5,
  STYPE_CFUNC    = 6,
  STYPE_SYM      = 7,
  STYPE_STR      = 8,
  STYPE_ERR      = 9,
  STYPE_CONS     = 10,
  STYPE_ARR      = 11,
  STYPE_INST     = 12,
  STYPE_SCOPE    = 13,
  STYPE_CODE_GEN = 14,
  STYPE_CODE     = 15,
  STYPE_KEY      = 16
};

typedef struct Snap_ Snap;
typedef struct SObject_ SObject;

typedef struct SValue_ {
  uint8_t type;
  union {
    bool b;
    SnapInt i;
    SnapFloat f;
    SObject* o;
    void (*c)(Snap* snap, const struct SValue_* args, int num_args, struct SValue_* result);
  };
} SValue;

typedef void (*SCFunc)(Snap* snap, const SValue* args, int num_args, SValue* result);

int snap_hash(SValue* val);
int snap_hash_str(const char* str, size_t len);
int snap_compare(SValue* val1, SValue* val2);

#endif

