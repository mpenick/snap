#ifndef SNAP_LEX_H
#define SNAP_LEX_H

#include <stddef.h>

enum {
  TK_INVALID,
  TK_TOO_BIG,
  TK_INT,
  TK_FLOAT,
  TK_STR,
  TK_ID,
  TK_KEY,
  TK_TRUE,
  TK_FALSE,
  TK_NIL,
  TK_DO,
  TK_DEF,
  TK_ELLIPSIS,
  TK_IF,
  TK_FN,
  TK_LET,
  TK_QUOTE,
  TK_RECUR,
  TK_SET,
  TK_RAISE, TK_TRY, TK_CATCH,
  TK_ADD, TK_SUB, TK_MUL, TK_DIV, TK_MOD,
  TK_LT, TK_LE, TK_GT, TK_GE, TK_EQ,
  TK_NOT,
  TK_EOF
};

typedef struct {
  const char* buf;
  size_t buf_size;
  const char* p;
  const char* mark;
  int line;
  char val[128];
} SnapLex;

#define snap_lex_mark(lex) lex->mark = lex->p
#define snap_lex_rewind(lex) lex->p = lex->mark

int snap_lex_next_token(SnapLex* lex);

#endif
