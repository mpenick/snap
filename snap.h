#ifndef SNAP_H
#define SNAP_H

struct Cons_;

enum {
  TYPE_INT,
  TYPE_FLOAT,
  TYPE_STR,
  TYPE_ID,
  TYPE_CONS
};

typedef struct {
  int type;
  union {
    int i;
    double f;
    const char* s;
    struct Cons_* c;
  };
} Value;

#define type(X) (X).type
#define val_i(X) (X).i
#define val_f(X) (X).f
#define val_s(X) (X).s
#define val_c(X) (X).c

typedef struct Cons_ {
  Value value;
  struct Cons_* next;
} Cons;

typedef struct Snap_ {
} Snap;

#endif
