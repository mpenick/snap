#include "snap_lex.h"

#include <string.h>

%%{
  machine lex;
  write data;
}%%

static int copy_value(SnapLex* lex, const char* ts, const char* te, int token) {
  size_t token_size = te - ts;
  if (token_size >= sizeof(lex->val)) { // need space for '\0'
    return TK_TOO_BIG;
  }
  memcpy(lex->val, ts, token_size);
  lex->val[token_size] = '\0';
  return token;
}

int snap_lex_next_token(SnapLex* lex) {
  int token = TK_INVALID;

  const char* p = lex->p;
  const char* pe = lex->buf + lex->buf_size;
  const char* ts;
  const char* te;
  const char* eof = pe;
  int cs, act;

  if (p == eof) return TK_EOF;

  lex->val[0] = '\0';

  %%{
    ws = [ \t];
    nl = '\r\n' | '\n';
    id = (alnum | [_\+\-\*/=,<>])+;
    integer = [\+\-]? digit+;
    floating = [\+\-]? ((digit+ ('.' digit+)?) | ('.' digit+) | (digit+ '.')) ([eE] [\+\-]? digit+)?;
    #string = ("\"" ([^\r\n\"] | "\\\"")* "\"") | ("'" ([^\r\n'] | "\\'")* "'");
    string = "\"" ([^\r\n\"] | "\\\"")* "\"";

    main := |*
      '(' => { token = (int)'('; fbreak; };
      ')' => { token = (int)')'; fbreak; };
      'do' => { token = TK_DO; fbreak; };
      'def' => { token = TK_DEF; fbreak; };
      'if' => { token = TK_IF; fbreak; };
      'fn' => { token = TK_FN; fbreak; };
      'let' => { token = TK_LET; fbreak; };
      'quote' => { token = TK_QUOTE; fbreak; };
      'set!' => { token = TK_SET; fbreak; };
      'recur' => { token = TK_RECUR; fbreak; };
      'true' => { token = TK_TRUE; fbreak; };
      'false' => { token = TK_FALSE; fbreak; };
      'nil' => { token = TK_NIL; fbreak; };
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

  if (token == TK_INVALID && p == eof) return TK_EOF;

  lex->p = p;

  return token;
}
