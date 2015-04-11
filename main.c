#include "lex.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct Cell_;

enum {
  TYPE_NIL,
  TYPE_INT,
  TYPE_FLOAT,
  TYPE_STR,
  TYPE_ID,
  TYPE_CELL
};

typedef struct {
  int type;
  union {
    int i;
    double f;
    const char* s;
    struct Cell_* c;
  } v;
} Value;

#define type(X) (X).type
#define val_i(X) (X).v.i
#define val_f(X) (X).v.f
#define val_s(X) (X).v.s
#define val_c(X) (X).v.c

typedef struct Cell_ {
  Value value;
  struct Cell_* next;
} Cell;

inline Value car(Cell* c) { return c->value; }
inline Cell* cdr(Cell* c) { return c->next;  }

Value parse_value(Lex* lex, int token);

Cell* parse_list(Lex* lex) {
  int token = lex_next_token(lex);
  Cell* c = NULL;
  while (token != ')') {
    c->v = parse_value(lex, token);
    token = lex_next_token(lex);
  }
  return NULL;
}

Value parse_value(Lex* lex, int token) {
  Value v;

  switch(token) {
    case '(':
      type(v) = TYPE_CELL;
      val_c(v) = parse_list(lex);
      break;
    case TK_INT:
      type(v) = TYPE_INT;
      val_i(v) = atoi(lex->value);
      break;
    case TK_FLOAT:
      type(v) = TYPE_FLOAT;
      val_f(v) = atof(lex->value);
      break;
    case TK_STR:
      type(v) = TYPE_STR;
      val_s(v) = strdup(lex->value);
      break;
    case TK_ID:
      type(v) = TYPE_ID;
      val_s(v) = strdup(lex->value);
      break;
    default:
      fprintf(stderr, "Parser error at %d\n", lex->line);
      break;
  }

  return v;
}

Value parse(Lex* lex) {
  return parse_value(lex, lex_next_token(lex));
}

int main() {
  const char* buf = "( + a_b b c 123 1.2 1.0e10 10e-1 -.0 -0. \"hi\")";

  Lex lex;
  lex.buf = buf;
  lex.buf_size = strlen(buf);
  lex.p = buf;
  lex.line = 0;

  int token;

  while ((token = lex_next_token(&lex)) != TK_EOF) {
    if (token == TK_INT) {
      printf("int(%s) %d\n", lex.value, atoi(lex.value));
    } else if (token == TK_FLOAT) {
      printf("float(%s) %f\n", lex.value, atof(lex.value));
    } else if (token == TK_STR) {
      printf("str %s\n", lex.value);
    } else {
      printf("other(%d) %s\n", token, lex.value);
    }
  }

  return 0;
}
