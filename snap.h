#ifndef SNAP_H
#define SNAP_H

#include "mstructs.h"

#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define check(a, e) (assert(a), (e))

#define is_undef(v) ((v).type == STYPE_UNDEF)
#define is_obj(v) ((v).type > STYPE_CFUNC)
#define is_nil(v) ((v).type == STYPE_NIL)
#define is_arr(v) ((v).type == STYPE_ARR)
#define is_bool(v) ((v).type == STYPE_BOOL)
#define is_bool_p(v) ((v)->type == STYPE_BOOL)
#define is_int(v) ((v).type == STYPE_INT)
#define is_int_p(v) ((v)->type == STYPE_INT)
#define is_float(v) ((v).type == STYPE_FLOAT)
#define is_float_p(v) ((v)->type == STYPE_FLOAT)
#define is_form(v) ((v).type == STYPE_FORM)
#define is_cfunc_p(v) ((v)->type == STYPE_CFUNC)
#define is_sym(v) ((v).type == STYPE_SYM)
#define is_str(v) ((v).type == STYPE_STR)
#define is_key(v) ((v).type == STYPE_KEY)
#define is_err(v) ((v).type == STYPE_ERR)
#define is_cons(v) ((v).type == STYPE_CONS)
#define is_code(v) ((v).type == STYPE_CODE)
#define is_code_p(v) ((v)->type == STYPE_CODE)
#define is_closure(v) ((v).type == STYPE_CLOSURE)
#define is_closure_p(v) ((v)->type == STYPE_CLOSURE)

#define as_arr(v) check(is_arr(v), (SArr*)(v).o)
#define as_str(v) check(is_str(v), (SStr*)(v).o)
#define as_sym(v) check(is_sym(v), (SSym*)(v).o)
#define as_key(v) check(is_key(v), (SKeyword*)(v).o)
#define as_err(v) check(is_err(v), (SErr*)(v).o)
#define as_cons(v) check(is_cons(v), (SCons*)(v).o)
#define as_code(v) check(is_code(v), (SCode*)(v).o)
#define as_code_p(v) check(is_code_p(v), (SCode*)(v)->o)
#define as_closure(v) check(is_closure(v), (SClosure*)(v).o)

#define is_empty(v) (is_cons(v) && as_cons(v) == NULL)

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

typedef long SnapInt;
typedef double SnapFloat;
typedef void (*SCFunc)(Snap* snap, const SValue* args, int num_args, SValue* result);

// Opcode in the low 8 bits, signed argument in the high 24 bits
typedef uint32_t SnapInst;

struct SValue_ {
  uint8_t type;
  union {
    bool b;
    SnapInt i;
    SnapFloat f;
    SObject* o;
    SCFunc c;
  };
};

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
};

SValue create_undef();
SValue create_nil();
SValue create_empty();
SValue create_bool(bool b);
SValue create_int(SnapInt i);
SValue create_float(SnapFloat f);
SValue create_cfunc(SCFunc c);

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
