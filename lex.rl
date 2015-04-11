#include "lex.h"

#include <string.h>

%%{
  machine lex;
  write data;
}%%

static int copy_value(Lex* lex, const char* ts, const char* te, int token) {
  size_t token_size = te - ts;
  if (token_size >= sizeof(lex->value)) { // need space for '\0'
    return TK_TOO_BIG;
  }
  memcpy(lex->value, ts, token_size);
  lex->value[token_size] = '\0';
  return token;
}

int lex_next_token(Lex* lex) {
  int token = TK_INVALID;

  const char* p = lex->p;
  const char* pe = lex->buf + lex->buf_size;
  const char* ts;
  const char* te;
  const char* eof = pe;
  int cs, act;

  if (p == eof) return TK_EOF;

  lex->value[0] = '\0';

  %%{
    ws = [ \t];
    nl = '\r\n' | '\n';
    id = (alnum | [_\+\-\*/=,])+;
    integer = [\+\-]? digit+;
    floating = [\+\-]? ((digit+ ('.' digit+)?) | ('.' digit+) | (digit+ '.')) ([eE] [\+\-]? digit+)?;
    #string = ("\"" ([^\r\n\"] | "\\\"")* "\"") | ("'" ([^\r\n'] | "\\'")* "'");
    string = "\"" ([^\r\n\"] | "\\\"")* "\"";

    main := |*
      '(' => { token = (int)'('; fbreak; };
      ')' => { token = (int)')'; fbreak; };
      integer => { token = copy_value(lex, ts, te, TK_INT); fbreak; };
      floating => { token = copy_value(lex, ts, te, TK_FLOAT); fbreak; };
      string => { token = copy_value(lex, ts + 1, te - 1, TK_STR); fbreak; };
      id => { token = copy_value(lex, ts, te, TK_ID); fbreak; };
      nl => { lex->line++; };
      ws => { /* Skip */ };
      any => { token = TK_INVALID; fbreak; };
    *|;

    write init;
    write exec;
  }%%

  lex->p = p;

  return token;
}
