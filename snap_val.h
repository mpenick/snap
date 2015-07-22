#ifndef SNAP_VAL_H
#define SNAP_VAL_H

#include <stdint.h>

enum {
  STYPE_NIL,
  STYPE_INT,
  STYPE_CFUNC,
  STYPE_FLOAT,
  STYPE_SYM,
  STYPE_STR,
  STYPE_ERR,
  STYPE_CONS,
  STYPE_HASH,
  STYPE_SCOPE,
  STYPE_LAMBDA
};

typedef struct Snap_ Snap;
typedef struct SObject_ SObject;
typedef struct SCons_ SCons;

typedef struct SValue_ {
  uint8_t type;
  union {
    int i;
    double f;
    SObject* o;
    struct SValue_ (*c)(Snap* snap, SCons* args);
  };
} SValue;

typedef SValue (*SFunc)(Snap* snap, SCons* args);

#endif

