#ifndef SNAP_H
#define SNAP_H

#include "snap_hash.h"

#include <stddef.h>
#include <stdint.h>

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

typedef struct {
  SOBJECT_FIELDS
  int code;
  SSymStr* msg;
} SErr;

struct SCons_ {
  SOBJECT_FIELDS
  SValue first;
  struct SCons_* rest;
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

typedef struct SFn_ {
  SOBJECT_FIELDS
  SSymStr* name;
  SCons* params;
  SCons* body;
} SFn;

struct Snap_ {
  SScope* scope;
  SObject** anchored;
  size_t anchored_capacity;
  SObject** anchored_top;
  size_t num_bytes_alloced;
  size_t num_bytes_alloced_last_gc;
  SObject* all;
  SObject* gray;
};

SSymStr* snap_str_new(Snap* snap, const char* str);
SSymStr* snap_sym_new(Snap* snap, const char* sym);
SErr* snap_err_new(Snap* snap, int code, const char* msg);
SCons* snap_cons_new(Snap* snap);
SHash* snap_hash_new(Snap* snap);
SScope* snap_scope_new(Snap* snap);
SFn* snap_fn_new(Snap* snap, SCons* params, SCons* body);

SObject* snap_push(Snap* snap, SObject* obj);
void snap_pop(Snap* snap);

void snap_def(Snap* snap, const char* name, SValue val);
void snap_def_cfunc(Snap* snap, const char* name, SCFunc cfunc);
SValue snap_exec(Snap* snap, const char* expr);
void snap_print(SValue value);

void snap_init(Snap* snap);
void snap_destroy(Snap* snap);

#endif
