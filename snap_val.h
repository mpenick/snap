#ifndef SNAP_VAL_H
#define SNAP_VAL_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef int64_t SnapInt;
typedef double SnapFloat;

#define STYPE_MAP(XX) \
  XX(STYPE_UNDEF, 0, "undef") \
  XX(STYPE_NIL, 1, "nil") \
  XX(STYPE_BOOL, 2, "bool") \
  XX(STYPE_INT, 3, "int") \
  XX(STYPE_FLOAT, 4, "float") \
  XX(STYPE_FORM, 5, "form") \
  XX(STYPE_CFUNC, 6, "cfunc") \
  XX(STYPE_SYM, 7, "sym") \
  XX(STYPE_STR, 8, "str") \
  XX(STYPE_ERR, 9, "err") \
  XX(STYPE_CONS, 10, "cons") \
  XX(STYPE_ARR, 11, "array") \
  XX(STYPE_INST, 12, "inst") \
  XX(STYPE_SCOPE, 13, "scope") \
  XX(STYPE_CODE_GEN, 14, "codegen") \
  XX(STYPE_CODE, 15, "code") \
  XX(STYPE_KEY, 16, "key")

enum {
#define XX(type, id, name) type,
  STYPE_MAP(XX)
#undef XX
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

