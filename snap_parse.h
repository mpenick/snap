// The reader (snap_parse.c): tokens, the lexer's state and parsing. The
// lexer itself is snap_lex.rh, which only snap_parse.c includes.
#ifndef SNAP_SNAP_PARSE_H
#define SNAP_SNAP_PARSE_H

#include "snap.h"

// Above 127, since characters like '(' are their own tokens
enum {
  TK_INVALID = 128,
  TK_TOO_BIG,
  TK_INT,
  TK_FLOAT,
  TK_STR,
  TK_ID,
  TK_KEY,
  TK_TRUE,
  TK_FALSE,
  TK_NIL,
  TK_BEGIN,
  TK_DEF,
  TK_DEFMACRO,
  TK_ELLIPSIS,
  TK_IF,
  TK_FN,
  TK_LET,
  TK_LOOP,
  TK_QUOTE,
  TK_RECUR,
  TK_SET,
  TK_PANIC,
  TK_TRY,
  TK_RECOVER,
  TK_ADD, TK_SUB, TK_MUL, TK_DIV, TK_MOD,
  TK_LT, TK_LE, TK_GT, TK_GE, TK_EQ,
  TK_AND, TK_OR, TK_NOT, TK_NE,
  TK_UNQUOTE, // ~x, never lexed
  TK_SPLICE,  // ~@x
  TK_INLINE,  // (#inline k coll), never lexed (see inline_seq())
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

int snap_lex_next_token(SnapLex* lex);

void parse_enter(Snap* snap, const char* err);
bool parse_form(Snap* snap, SnapLex* lex, SValue* result);
void parse_raise(Snap* snap, const char* err, const char* format, ...);

#endif // SNAP_SNAP_PARSE_H
