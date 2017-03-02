#ifndef SNAP_H
#define SNAP_H

#include "mstructs.h"

#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>

#define check(a, e) (assert(a), (e))

#define is_undef(v) ((v).type == STYPE_UNDEF)
#define is_undef_p(v) ((v)->type == STYPE_UNDEF)
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
#define is_cfunc(v) ((v).type == STYPE_CFUNC)
#define is_cfunc_p(v) ((v)->type == STYPE_CFUNC)
#define is_sym(v) ((v).type == STYPE_SYM)
#define is_sym_p(v) ((v)->type == STYPE_SYM)
#define is_str(v) ((v).type == STYPE_STR)
#define is_str_p(v) ((v)->type == STYPE_STR)
#define is_key(v) ((v).type == STYPE_KEY)
#define is_key_p(v) ((v)->type == STYPE_KEY)
#define is_err(v) ((v).type == STYPE_ERR)
#define is_err_p(v) ((v)->type == STYPE_ERR)
#define is_cons(v) ((v).type == STYPE_CONS)
#define is_hash(v) ((v).type == STYPE_HASH)
#define is_scope(v) ((v).type == STYPE_SCOPE)
#define is_fn(v) ((v).type == STYPE_FN)
#define is_inst(v) ((v).type == STYPE_INST)
#define is_code(v) ((v).type == STYPE_CODE)
#define is_code_p(v) ((v)->type == STYPE_CODE)
#define is_closure_p(v) ((v)->type == STYPE_CLOSURE)
#define is_closed_desc(v) ((v).type == STYPE_CLOSED_DESC)

#define as_arr(v) check(is_arr(v), (SArr*)(v).o)
#define as_str(v) check(is_str(v), (SSymStr*)(v).o)
#define as_str_p(v) check(is_str_p(v), (SSymStr*)(v)->o)
#define as_sym(v) check(is_sym(v), (SSymStr*)(v).o)
#define as_sym_p(v) check(is_sym_p(v), (SSymStr*)(v)->o)
#define as_key(v) check(is_key(v), (SKeyword*)(v).o)
#define as_key_p(v) check(is_key_p(v), (SKeyword*)(v)->o)
#define as_err(v) check(is_err(v), (SErr*)(v).o)
#define as_err_p(v) check(is_err_p(v), (SErr*)(v)->o)
#define as_cons(v) check(is_cons(v), (SCons*)(v).o)
#define as_code(v) check(is_code(v), (SCode*)(v).o)
#define as_code_p(v) check(is_code_p(v), (SCode*)(v)->o)
#define as_closed_desc(v) check(is_closed_desc(v), (SClosedDesc*)(v).o)
#define as_inst(v) check(is_inst(v), (SInst*)(v).o)
#define as_fn(v) check(is_fn(v), (SFn*)(v).o)

#define SNAP_MAX_BLOCKS 16

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
  XX(STYPE_ERR, 10, "err") \
  XX(STYPE_CONS, 11, "cons") \
  XX(STYPE_ARR, 12, "array") \
  XX(STYPE_INST, 13, "inst") \
  XX(STYPE_SCOPE, 14, "scope") \
  XX(STYPE_CODE_GEN, 15, "codegen") \
  XX(STYPE_CODE, 16, "code") \
  XX(STYPE_KEY, 17, "key") \
  XX(STYPE_TEMPSTR, 18, "tempstr") \
  XX(STYPE_CLOSURE, 19, "closure") \
  XX(STYPE_CLOSED_DESC, 20, "closeddesc")

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
  struct SObject_* gc_next;     \
  struct SObject_* gc_gray_next;

struct SObject_ {
  SOBJECT_FIELDS
};

typedef struct SSymStr_ {
  SOBJECT_FIELDS
  size_t len;
  char data[1];
} SSymStr;

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

typedef struct SnapJumpArg_ {
  int dir;
  MList* dest;
} SnapJumpArg;

typedef struct SInst_ {
  SOBJECT_FIELDS
  MList list;
  int opcode;
  union {
    int arg;
    SnapJumpArg jump_arg;
  };
  SValue arg_data;
} SInst;

typedef struct SScope_ {
  SOBJECT_FIELDS
  MList* top;
  SnapHash local_names;
  SnapVec param_names;
  struct SScope_* up;
}  SScope;

typedef struct SClosedDesc_ {
  SOBJECT_FIELDS
  bool islocal;
  int index;
  SSymStr* name;
} SClosedDesc;

typedef struct SnapClosed_ {
  SValue* value;
  SValue closed;
} SnapClosed;

typedef struct SClosure_ {
  SOBJECT_FIELDS
  SCode* code;
  struct SClosure_* next;
  SnapClosed closed[0];
} SClosure;

typedef struct SCodeGen_ {
  SOBJECT_FIELDS
  MList insts;
  int insts_count;
  SScope* scope;
  SnapHash constants;
  SnapHash global_names;
  SnapHash closed_names;
  SnapVec closed_descs;
  SnapVec param_names;
  int num_locals;
  int num_results;
  bool is_tail;
  struct SCodeGen_* up;
} SCodeGen;

struct SCode_ {
  SOBJECT_FIELDS
  int* insts;
  SArr* global_names;
  SArr* constants;
  SArr* closed_descs;
  int num_args;
  int num_locals;
  int max_stack_size;
  int insts_count;
  MList insts_debug;
};

typedef struct SnapBlock_ {
  int insts_offset;
  int stack_offset;
} SnapBlock;

typedef struct SnapFrame_ {
  int* pc;
  SCode* code;
  SClosure* closure;
  SClosure* enclosed;
  SValue* stack_base;
  SValue* stack_top;
  SnapBlock blocks[SNAP_MAX_BLOCKS];
  int blocks_top;
} SnapFrame;

struct Snap_ {
  SnapHash globals;
  SnapHash keywords;
  SValue* stack;
  SValue* stack_end;
  int stack_size;
  struct { MVEC_FIELDS(SnapFrame); } frames;
  struct { MVEC_FIELDS(SObject*); } anchors;
  size_t num_bytes_alloced;
  size_t num_bytes_alloced_last_gc;
  SObject* all;
  SObject* gray;
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
SValue create_obj(SObject* o);

SSymStr* snap_str_new(Snap* snap, const char* str);
SSymStr* snap_sym_new(Snap* snap, const char* sym);
SErr* snap_err_new(Snap* snap, SValue err, SValue msg);
SErr* snap_err_new_str(Snap* snap, const char* code, const char* msg);
SCons* snap_cons_new(Snap* snap);
SInst* snap_inst_new(Snap* snap, int opcode);
SScope* snap_scope_new(Snap* snap, SScope* up);
SCodeGen* snap_code_gen_new(Snap* snap, SCodeGen* up);

SObject* snap_anchor(Snap* snap, SObject* obj);
void snap_release(Snap* snap);

void snap_def(Snap* snap, const char* name, SValue val);
void snap_def_cfunc(Snap* snap, const char* name, SCFunc cfunc);
SValue snap_exec(Snap* snap, const char* str);
void snap_print(SValue value);

void snap_init(Snap* snap);
void snap_destroy(Snap* snap);

#endif
