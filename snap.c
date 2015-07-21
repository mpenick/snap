#include "snap_lex.h"

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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
  } v;
} Value;

#define type(X) (X).type
#define val_i(X) (X).v.i
#define val_f(X) (X).v.f
#define val_s(X) (X).v.s
#define val_c(X) (X).v.c

typedef struct Cons_ {
  Value value;
  struct Cons_* next;
} Cons;

typedef struct Snap_ {
} Snap;

typedef struct Entry_ {
  const char* key;
  Value value;
} Entry;

typedef struct Hash_ {
  Entry* entries;
  size_t capacity;
  size_t count;
} Hash;

inline Value car(Cons* c) { return c->value; }
inline Cons* cdr(Cons* c) { return c->next;  }

Cons* create_cell(Value value) {
  Cons* cell = (Cons*)malloc(sizeof(Cons));
  cell->value = value;
  cell->next = NULL;
  return cell;
}

Value parse_value(Lex* lex, int token);

Cons* parse_list(Lex* lex) {
  int token = lex_next_token(lex);
  Cons* first = NULL;
  Cons** cell = &first;
  while (token != ')' && token != TK_EOF) {
    *cell = create_cell(parse_value(lex, token));
    cell = &(*cell)->next;
    token = lex_next_token(lex);
  }
  if (token != ')') {
    fprintf(stderr, "Parser error at %d\n", lex->line);
    exit(-1);
  }
  return first;
}

Value parse_value(Lex* lex, int token) {
  Value v;

  switch(token) {
    case '(':
      type(v) = TYPE_CONS;
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
      exit(-1);
      break;
  }

  return v;
}

Value parse(Lex* lex) {
  return parse_value(lex, lex_next_token(lex));
}

void print_cell(Cons* cell);

void print_value(Value value) {
  switch(type(value)) {
    case TYPE_INT:
      printf("%d", val_i(value));
      break;
    case TYPE_FLOAT:
      printf("%f", val_f(value));
      break;
    case TYPE_STR:
      printf("%s", val_s(value));
      break;
    case TYPE_ID:
      printf("%s", val_s(value));
      break;
    case TYPE_CONS:
      printf("\n(");
      print_cell(val_c(value));
      printf(")");
      break;
  }
}

void print_cell(Cons* cell) {
  int space = 0;
  while (cell != NULL) {
    if (space) printf(" ");
    space = 1;
    print_value(cell->value);
    cell = cell->next;
  }
}

int main(int argc, char** argv) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s <str>\n", argv[0]);
    exit(-1);
  }

  //const char* buf = "( + a_b b c 123 1.2 1.0e10 10e-1 -.0 -0. \"hi\")";

  Lex lex;
  lex.buf = argv[1];
  lex.buf_size = strlen(lex.buf);
  lex.p = lex.buf;
  lex.line = 0;

  Value v = parse(&lex);
  print_value(v);
  printf("\n");


  //int token;

  //while ((token = lex_next_token(&lex)) != TK_EOF) {
  //  if (token == TK_INT) {
  //    printf("int(%s) %d\n", lex.value, atoi(lex.value));
  //  } else if (token == TK_FLOAT) {
  //    printf("float(%s) %f\n", lex.value, atof(lex.value));
  //  } else if (token == TK_STR) {
  //    printf("str %s\n", lex.value);
  //  } else {
  //    printf("other(%d) %s\n", token, lex.value);
  //  }
  //}

  return 0;
}
