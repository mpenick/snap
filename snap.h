#ifndef SNAP_H
#define SNAP_H

#include "mstructs.h"

#include <assert.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define check(a, e) (assert(a), (e))

#define STYPE_MAPPING(XX) \
  XX(STYPE_UNDEF, 0, "undef") \
  XX(STYPE_DELETED, 1, "deleted") \
  XX(STYPE_NIL, 2, "nil") \
  XX(STYPE_BOOL, 3, "bool") \
  XX(STYPE_INT, 4, "int") \
  XX(STYPE_FLOAT, 5, "float") \
  XX(STYPE_FORM, 6, "form") \
  XX(STYPE_CFUNC, 7, "cfunc") \
  XX(STYPE_SYM, 8, "sym") \
  XX(STYPE_STR, 9, "str") \
  XX(STYPE_KEY, 10, "key") \
  XX(STYPE_ERR, 11, "err") \
  XX(STYPE_CONS, 12, "cons") \
  XX(STYPE_ARR, 13, "array") \
  XX(STYPE_CODE, 14, "code") \
  XX(STYPE_CLOSURE, 15, "closure") \
  XX(STYPE_UPVAL, 16, "upval") \
  XX(STYPE_TEMPSTR, 17, "tempstr")

enum {
#define XX(type, id, name) type,
  STYPE_MAPPING(XX)
#undef XX
};

typedef struct Snap_ Snap;
typedef struct SCode_ SCode;
typedef struct SObject_ SObject;
typedef struct SValue_ SValue;

typedef int64_t SnapInt;
typedef double SnapFloat;
// Returns false to panic, with *result set to the error
typedef bool (*SCFunc)(Snap* snap, const SValue* args, int num_args, SValue* result);

// Opcode in the low 8 bits, signed argument in the high 24 bits
typedef uint32_t SnapInst;

// A value is one tagged word:
//   ...xxx1  63-bit integer (i << 1 | 1)
//   ...x000  pointer to a GC object, whose header holds its type (floats too)
//   ...x010  undef, deleted, nil, false, true or the empty list (n << 3 | 2)
//   ...x100  special form (token << 3 | 4)
//   ...x110  C function (pointer << 3 | 6; user space pointers fit in 61 bits)
struct SValue_ {
  uint64_t bits;
};

#define TAG_MASK 7
#define TAG_OBJ 0
#define TAG_SPECIAL 2
#define TAG_FORM 4
#define TAG_CFUNC 6

#define SV_SPECIAL(n) (((uint64_t)(n) << 3) | TAG_SPECIAL)
#define SV_UNDEF SV_SPECIAL(0)
#define SV_DELETED SV_SPECIAL(1)
#define SV_NIL SV_SPECIAL(2)
#define SV_FALSE SV_SPECIAL(3)
#define SV_TRUE SV_SPECIAL(4)
#define SV_EMPTY SV_SPECIAL(5)

#define SNAP_INT_MAX ((SnapInt)(((uint64_t)1 << 62) - 1))
#define SNAP_INT_MIN (-SNAP_INT_MAX - 1)

int snap_hash(const SValue* val);
int snap_compare(const SValue* val1, const SValue* val2);

typedef struct SnapVec_ {
  MVEC_FIELDS(SValue);
} SnapVec;

typedef struct {
  MHASH_ENTRY_FIELDS(SValue);
  SValue val;
} SnapHashEntry;

typedef struct {
  MHASH_FIELDS(SnapHashEntry, SValue);
} SnapHash;

#define SOBJECT_FIELDS          \
  uint8_t type;                 \
  uint8_t mark;                 \
  struct SObject_* gc_next;

struct SObject_ {
  SOBJECT_FIELDS
};

typedef struct SFloat_ {
  SOBJECT_FIELDS
  SnapFloat f;
} SFloat;

typedef struct SStr_ {
  SOBJECT_FIELDS
  size_t len;
  char data[1];
} SStr;

// Symbols are interned and hold their global binding (undef when unbound)
typedef struct SSym_ {
  SOBJECT_FIELDS
  SValue value;
  size_t len;
  char data[1];
} SSym;

typedef struct SKeyword_ {
  SOBJECT_FIELDS
  int id;
  size_t len;
  char data[1];
} SKeyword;

typedef struct STempStr_ {
  SOBJECT_FIELDS
  size_t len;
  const char* data;
} STempStr;

typedef struct SErr_ {
  SOBJECT_FIELDS
  SValue err;
  SValue msg;
} SErr;

typedef struct SCons_ {
  SOBJECT_FIELDS
  SValue first;
  SValue rest;
} SCons;

typedef struct SArr_ {
  SOBJECT_FIELDS
  int len;
  SValue data[1];
} SArr;

typedef struct SUpval_ {
  SOBJECT_FIELDS
  SValue* value; // Points into the stack while open, at `closed` once closed
  SValue closed;
  struct SUpval_* next; // Next open upvalue (sorted by descending stack address)
} SUpval;

typedef struct SClosure_ {
  SOBJECT_FIELDS
  SCode* code;
  int num_upvals;
  SUpval* upvals[0];
} SClosure;

struct SCode_ {
  SOBJECT_FIELDS
  SnapInst* insts;
  SValue* constants;
  uint32_t* upvals; // (index << 1) | islocal, islocal means a slot in the enclosing frame
  int insts_count;
  int num_constants;
  int num_upvals;
  int num_args;
  int num_locals;
  int max_stack_size;
};

typedef struct SnapFrame_ {
  SnapInst* pc;
  SCode* code;
  SClosure* closure;
  SValue* stack_base;
  SValue* stack_top;
} SnapFrame;

struct Snap_ {
  SnapHash symbols;
  SnapHash keywords;
  SValue* stack;
  SValue* stack_end;
  int stack_size;
  struct { MVEC_FIELDS(SnapFrame); } frames;
  struct { MVEC_FIELDS(SObject*); } anchors;
  size_t num_bytes_alloced;
  size_t next_gc; // Collect when num_bytes_alloced reaches this
  SObject* all;
  struct { MVEC_FIELDS(SObject*); } gray; // Marked objects whose children still need marking
  SUpval* open_upvals;
  struct SnapFnState_* fn_state; // Innermost function being compiled
  SErr* cause;
  jmp_buf jmp;
  bool dump_code; // Print the bytecode from snap_exec() before running it
};

#define is_int(v) ((v).bits & 1)
#define is_obj(v) (((v).bits & TAG_MASK) == TAG_OBJ)
#define is_undef(v) ((v).bits == SV_UNDEF)
#define is_nil(v) ((v).bits == SV_NIL)
#define is_false(v) ((v).bits == SV_FALSE)
#define is_bool(v) (is_false(v) || (v).bits == SV_TRUE)
#define is_empty(v) ((v).bits == SV_EMPTY)
#define is_form(v) (((v).bits & TAG_MASK) == TAG_FORM)
#define is_cfunc(v) (((v).bits & TAG_MASK) == TAG_CFUNC)
#define is_obj_type(v, t) (is_obj(v) && as_obj(v)->type == (t))
#define is_float(v) is_obj_type(v, STYPE_FLOAT)
#define is_sym(v) is_obj_type(v, STYPE_SYM)
#define is_str(v) is_obj_type(v, STYPE_STR)
#define is_key(v) is_obj_type(v, STYPE_KEY)
#define is_err(v) is_obj_type(v, STYPE_ERR)
#define is_arr(v) is_obj_type(v, STYPE_ARR)
#define is_code(v) is_obj_type(v, STYPE_CODE)
#define is_closure(v) is_obj_type(v, STYPE_CLOSURE)
#define is_cons(v) (is_empty(v) || is_obj_type(v, STYPE_CONS))

#define as_obj(v) ((SObject*)(uintptr_t)(v).bits)
#define as_int(v) check(is_int(v), (SnapInt)(v).bits >> 1)
#define as_float(v) check(is_float(v), ((SFloat*)as_obj(v))->f)
#define as_form(v) check(is_form(v), (int)((v).bits >> 3))
#define as_cfunc(v) check(is_cfunc(v), (SCFunc)(uintptr_t)((v).bits >> 3))
#define as_str(v) check(is_str(v), (SStr*)as_obj(v))
#define as_sym(v) check(is_sym(v), (SSym*)as_obj(v))
#define as_key(v) check(is_key(v), (SKeyword*)as_obj(v))
#define as_err(v) check(is_err(v), (SErr*)as_obj(v))
#define as_arr(v) check(is_arr(v), (SArr*)as_obj(v))
#define as_code(v) check(is_code(v), (SCode*)as_obj(v))
#define as_closure(v) check(is_closure(v), (SClosure*)as_obj(v))
#define as_cons(v) check(is_cons(v), is_empty(v) ? NULL : (SCons*)as_obj(v)) // NULL for ()

static inline SValue snap_val(uint64_t bits) {
  SValue val;
  val.bits = bits;
  return val;
}

static inline SValue create_undef(void) { return snap_val(SV_UNDEF); }
static inline SValue create_nil(void) { return snap_val(SV_NIL); }
static inline SValue create_empty(void) { return snap_val(SV_EMPTY); }
static inline SValue create_bool(bool b) { return snap_val(b ? SV_TRUE : SV_FALSE); }
static inline SValue create_int(SnapInt i) { return snap_val(((uint64_t)i << 1) | 1); }
static inline SValue create_form(int token) { return snap_val(((uint64_t)token << 3) | TAG_FORM); }
static inline SValue create_cfunc(SCFunc c) { return snap_val(((uint64_t)(uintptr_t)c << 3) | TAG_CFUNC); }
static inline SValue create_object(SObject* o) { return snap_val((uint64_t)(uintptr_t)o); }
#define create_obj(o) create_object((SObject*)(o))
SValue create_float(Snap* snap, SnapFloat f); // Allocates

static inline int snap_type(SValue val) {
  static const uint8_t specials[] = {
    STYPE_UNDEF, STYPE_DELETED, STYPE_NIL, STYPE_BOOL, STYPE_BOOL, STYPE_CONS
  };
  if (is_int(val)) return STYPE_INT;
  switch (val.bits & TAG_MASK) {
    case TAG_OBJ: return as_obj(val)->type;
    case TAG_SPECIAL: return specials[val.bits >> 3];
    case TAG_FORM: return STYPE_FORM;
    default: return STYPE_CFUNC;
  }
}

SStr* snap_str_new(Snap* snap, const char* str);
SSym* snap_sym_new(Snap* snap, const char* name);
SErr* snap_err_new(Snap* snap, SValue err, SValue msg);
SCons* snap_cons_new(Snap* snap);

SObject* snap_anchor(Snap* snap, SObject* obj);
void snap_release(Snap* snap);

void snap_def(Snap* snap, const char* name, SValue val);
void snap_def_cfunc(Snap* snap, const char* name, SCFunc cfunc);
SValue snap_exec(Snap* snap, const char* str);
void snap_print(SValue value);

void snap_init(Snap* snap);
void snap_destroy(Snap* snap);

#endif
