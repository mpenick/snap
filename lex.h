#ifndef LEX_H
#define LEX_H

#include <stddef.h>

enum {
  TK_INVALID,
  TK_TOO_BIG,
  TK_INT,
  TK_FLOAT,
  TK_STR,
  TK_ID,
  TK_EOF
};

typedef struct {
  const char* buf;
  size_t buf_size;
  const char* p;
  int line;
  char value[128];
} Lex;

int lex_next_token(Lex* lex);

#endif
