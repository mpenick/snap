#ifndef SNAP_H
#define SNAP_H

#include "snap_hash.h"

#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define check(a, e) (assert(a), (e))

#define is_undef(v) ((v).type == STYPE_UNDEF)
#define is_obj(v) ((v).type > STYPE_CFUNC)
#define is_nil(v) ((v).type == STYPE_NIL)
#define is_bool(v) ((v).type == STYPE_BOOL)
#define is_int(v) ((v).type == STYPE_INT)
#define is_float(v) ((v).type == STYPE_FLOAT)
#define is_form(v) ((v).type == STYPE_FORM)
#define is_cfunc(v) ((v).type == STYPE_CFUNC)
#define is_sym(v) ((v).type == STYPE_SYM)
#define is_str(v) ((v).type == STYPE_STR)
#define is_err(v) ((v).type == STYPE_ERR)
#define is_cons(v) ((v).type == STYPE_CONS)
#define is_hash(v) ((v).type == STYPE_HASH)
#define is_scope(v) ((v).type == STYPE_SCOPE)
#define is_fn(v) ((v).type == STYPE_FN)

#define as_sym(v) check(is_sym(v), (SSymStr*)(v).o)
#define as_str(v) check(is_str(v), (SSymStr*)(v).o)
#define as_err(v) check(is_err(v), (SErr*)(v).o)
#define as_cons(v) check(is_cons(v), (SCons*)(v).o)
#define as_hash(v) check(is_hash(v), (SHash*)(v).o)
#define as_fn(v) check(is_fn(v), (SFn*)(v).o)

#define SOBJECT_FIELDS     \
  uint8_t type;            \
  uint8_t mark;            \
  struct SObject_* next;   \
  struct SObject_* gc_next;

struct SObject_ {
  SOBJECT_FIELDS
};

typedef struct {
  SOBJECT_FIELDS
  size_t len;
  char data[0];
} SSymStr;

typedef struct SErr_ {
  SOBJECT_FIELDS
  int code;
  SSymStr* msg;
  struct SErr_* inner;
} SErr;

struct SCons_ {
  SOBJECT_FIELDS
  SValue first;
  SValue rest;
};

typedef struct {
  SOBJECT_FIELDS
  SnapHash table;
} SHash;

typedef struct SScope_ {
  SOBJECT_FIELDS
  SnapHash vars;
  SCons* cons;
  struct SScope_* up;
} SScope;

typedef struct {
  SOBJECT_FIELDS
  int n;
  SSymStr* name;
  SScope* scope;
  SCons* params;
  SCons* body;
} SFn;

typedef struct SnapTry_ {
  jmp_buf buf;
  struct SnapTry_* up;
} SnapTry;

typedef struct SnapFrame_ {
  SScope* scope;
  struct SnapFrame_* up;
} SnapFrame;

#define SNAP_NODE_FIELDS  \
  struct SnapNode_* prev; \
  struct SnapNode_* next;

typedef struct SnapNode_ {
  SNAP_NODE_FIELDS
} SnapNode;

typedef struct SnapList_ {
  SNAP_NODE_FIELDS
  int count;
} SnapList;

typedef struct SnapInst_ {
  SNAP_NODE_FIELDS
  int opcode;
  int arg;
} SnapInst;

typedef struct SnapScope_ {
  SnapHash local_names;
  struct SnapScope_* up;
}  SnapScope;

typedef struct SnapCodeGen_ {
  SnapList insts;
  SnapScope* scope;
  SnapHash constants;
  SnapHash global_names;
  int num_locals;
  int max_stack_size;
  bool is_tail;
  struct SnapCodeGen_* up;
} SnapCodeGen;

struct Snap_ {
  SnapHash globals;
  SCons* tail;
  SObject** anchored;
  int anchored_capacity;
  int anchored_top;
  SnapFrame* frame;
  SnapTry* trying;
  SErr* cause;
  size_t num_bytes_alloced;
  size_t num_bytes_alloced_last_gc;
  SObject* all;
  SObject* gray;
  SnapFrame bottom_frame;
  SnapCodeGen* code_gen;
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
SErr* snap_err_new(Snap* snap, int code, const char* msg);
SCons* snap_cons_new(Snap* snap);
SHash* snap_hash_new(Snap* snap);
SScope* snap_scope_new(Snap* snap);
SFn* snap_fn_new(Snap* snap, int n, SCons* params, SCons* body);

SObject* snap_push(Snap* snap, SObject* obj);
void snap_pop(Snap* snap);

void snap_def(Snap* snap, const char* name, SValue val);
void snap_def_cfunc(Snap* snap, const char* name, SCFunc cfunc);
void snap_throw(Snap* snap, int code, const char* format, ...);
SValue snap_exec(Snap* snap, const char* expr);
void snap_print(SValue value);

void snap_parse(Snap* snap, const char* expr);

void snap_init(Snap* snap);
void snap_destroy(Snap* snap);

#endif
