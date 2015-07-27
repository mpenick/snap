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
  TK_THROW,
  TK_TRY,
  TK_EOF
};

typedef struct {
  const char* buf;
  size_t buf_size;
  const char* p;
  int line;
  char val[128];
} SnapLex;

int snap_lex_next_token(SnapLex* lex);

#endif
