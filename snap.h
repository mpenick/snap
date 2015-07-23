#ifndef SNAP_H
#define SNAP_H

#include "snap_hash.h"

#include <stddef.h>
#include <stdint.h>

enum {
  SERR_PARSE
};

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
  struct SScope_* up;
} SScope;

typedef struct SFn_ {
  SOBJECT_FIELDS
  SCons* params;
  SCons* body;
  SScope* scope;
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

void snap_init(Snap* snap);
void snap_destroy(Snap* snap);

void snap_def(Snap* snap, const char* name, SValue val);
void snap_def_func(Snap* snap, const char* name, SCFunc func);
SValue snap_exec(Snap* snap, const char* expr);

SSymStr* snap_str_new(Snap* snap, const char* str);
SSymStr* snap_sym_new(Snap* snap, const char* sym);
SErr* snap_err_new(Snap* snap, int code, const char* msg);
SCons* snap_cons_new(Snap* snap);
SHash* snap_hash_new(Snap* snap);
SScope* snap_scope_new(Snap* snap);
SFn* snap_fn_new(Snap* snap, SCons* params, SCons* body);

SObject* snap_push(Snap* snap, SObject* obj);
void snap_pop(Snap* snap);


#endif
