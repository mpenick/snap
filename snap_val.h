#ifndef SNAP_VAL_H
#define SNAP_VAL_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef long SnapInt;
typedef double SnapFloat;

enum {
  STYPE_UNDEF,
  STYPE_NIL,
  STYPE_BOOL,
  STYPE_INT,
  STYPE_FLOAT,
  STYPE_FORM,
  STYPE_CFUNC,
  STYPE_SYM,
  STYPE_STR,
  STYPE_ERR,
  STYPE_CONS,
  STYPE_HASH,
  STYPE_SCOPE,
  STYPE_FN
};

typedef struct Snap_ Snap;
typedef struct SObject_ SObject;
typedef struct SCons_ SCons;

typedef struct SValue_ {
  uint8_t type;
  union {
    bool b;
    SnapInt i;
    SnapFloat f;
    SObject* o;
    struct SValue_ (*c)(Snap* snap, SCons* args);
  };
} SValue;

typedef SValue (*SCFunc)(Snap* snap, SCons* args);

int snap_hash(SValue val);
int snap_compare(SValue val1, SValue val2);

#endif

