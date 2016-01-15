#ifndef SNAP_H
#define SNAP_H

#include "snap_hash.h"
#include "snap_vec.h"

#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define check(a, e) (assert(a), (e))

#define is_undef(v) ((v).type == STYPE_UNDEF)
#define is_undef_p(v) ((v)->type == STYPE_UNDEF)
#define is_obj(v) ((v).type > STYPE_CFUNC)
#define is_nil(v) ((v).type == STYPE_NIL)
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
#define as_inst(v) check(is_inst(v), (SInst*)(v).o)
#define as_fn(v) check(is_fn(v), (SFn*)(v).o)

#define SNAP_MAX_BLOCKS 16

typedef struct SnapNode_ {
  struct SnapNode_* prev;
  struct SnapNode_* next;
} SnapNode;

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
  char data[0];
} SSymStr;

typedef struct SKeyword_ {
  SOBJECT_FIELDS
  int id;
  size_t len;
  char data[0];
} SKeyword;

typedef struct SErr_ {
  SOBJECT_FIELDS
  SKeyword* err;
  SValue msg;
} SErr;

typedef struct SCons_ {
  SOBJECT_FIELDS
  SValue first;
  SValue rest;
} SCons;

typedef struct SArr_ {
  SOBJECT_FIELDS
  size_t len;
  SValue data[0];
} SArr;

typedef struct SnapJumpArg_ {
  int dir;
  SnapNode* dest;
} SnapJumpArg;

typedef struct SInst_ {
  SOBJECT_FIELDS
  SnapNode list;
  int opcode;
  union {
    int arg;
    SnapJumpArg jump_arg;
  };
} SInst;

typedef struct SScope_ {
  SOBJECT_FIELDS
  SnapHash local_names;
  struct SScope_* up;
}  SScope;

typedef struct SCodeGen_ {
  SOBJECT_FIELDS
  SnapNode insts;
  int insts_count;
  SScope* scope;
  SnapHash constants;
  SnapHash global_names;
  SnapVec param_names;
  int num_locals;
  int num_results;
  bool is_tail;
  struct SCodeGen_* up;
} SCodeGen;

typedef struct SCode_ {
  SOBJECT_FIELDS
  int* insts;
  SArr* global_names;
  SArr* constants;
  int num_locals;
  int max_stack_size;
  int insts_count;
} SCode;

typedef struct SnapBlock_ {
  int insts_offset;
  int stack_offset;
} SnapBlock;

typedef struct SnapFrame_ {
  int* pc;
  SCode* code;
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
  SnapFrame* frames;
  int frames_top;
  int frames_size;
  SObject** anchored;
  int anchored_capacity;
  int anchored_top;
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
SErr* snap_err_new(Snap* snap, SKeyword* err, SValue msg);
SErr* snap_err_new_str(Snap* snap, const char* code, const char* msg);
SCons* snap_cons_new(Snap* snap);
SInst* snap_inst_new(Snap* snap, int opcode);
SScope* snap_scope_new(Snap* snap, SScope* up);
SCodeGen* snap_code_gen_new(Snap* snap, SCodeGen* up);

SObject* snap_anchor(Snap* snap, SObject* obj);
void snap_release(Snap* snap);

void snap_define(Snap* snap, const char* name, SValue val);
void snap_def_cfunc(Snap* snap, const char* name, SCFunc cfunc);
SValue snap_exec(Snap* snap, const char* expr);
void snap_print(SValue value);

void snap_init(Snap* snap);
void snap_destroy(Snap* snap);

#endif
