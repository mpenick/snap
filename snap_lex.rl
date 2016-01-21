#include "snap_lex.h"

#include <string.h>
#include <stdio.h>

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
    comment = ";" [^\r\n]*;
    nl = '\r\n' | '\n';
    id = (alnum | [_\+\-\*\?/=,<>!])+;
    key = ':' id ;
    chars = '\'' | '(' | ')' | '[' | ']' | '{' | '}';
    integer = [\+\-]? digit+;
    floating = [\+\-]? ((digit+ ('.' digit+)?) | ('.' digit+) | (digit+ '.')) ([eE] [\+\-]? digit+)?;
    string = "\"" ([^\r\n\"] | "\\\"")* "\"";

    main := |*
      'do' => { token = TK_DO; fbreak; };
      'def' | 'define' => { token = TK_DEF; fbreak; };
      'if' => { token = TK_IF; fbreak; };
      'fn' => { token = TK_FN; fbreak; };
      'let' => { token = TK_LET; fbreak; };
      'loop' => { token = TK_LOOP; fbreak; };
      'quote' => { token = TK_QUOTE; fbreak; };
      'recur' => { token = TK_RECUR; fbreak; };
      'set!' => { token = TK_SET; fbreak; };
      'raise' => { token = TK_RAISE; fbreak; };
      'try' => { token = TK_TRY; fbreak; };
      'catch' => { token = TK_CATCH; fbreak; };
      'true' => { token = TK_TRUE; fbreak; };
      'false' => { token = TK_FALSE; fbreak; };
      'nil' => { token = TK_NIL; fbreak; };
      '...' => { token = TK_ELLIPSIS; fbreak; };
      '+' => { token = TK_ADD; fbreak; };
      '-' => { token = TK_SUB; fbreak; };
      '*' => { token = TK_MUL; fbreak; };
      '/' => { token = TK_DIV; fbreak; };
      '%' => { token = TK_MOD; fbreak; };
      '<' => { token = TK_LT; fbreak; };
      '<=' => { token = TK_LE; fbreak; };
      '>' => { token = TK_GT; fbreak; };
      '>=' => { token = TK_GE; fbreak; };
      '=' => { token = TK_EQ; fbreak; };
      'not' => { token = TK_NOT; fbreak; };
      chars => { token = *ts; fbreak; };
      integer => { token = copy_value(lex, ts, te, TK_INT); fbreak; };
      floating => { token = copy_value(lex, ts, te, TK_FLOAT); fbreak; };
      string => { token = copy_value(lex, ts + 1, te - 1, TK_STR); fbreak; };
      id => { token = copy_value(lex, ts, te, TK_ID); fbreak; };
      key => { token = copy_value(lex, ts + 1, te, TK_KEY); fbreak; };
      nl => { lex->line++; };
      ws => { /* Skip */ };
      comment => { /* Skip */ };
      any => { token = TK_INVALID; fbreak; };
    *|;

    write init;
    write exec;
  }%%

  token = token == TK_INVALID && p == eof ? TK_EOF : token;
  lex->p = p;

  return token;
}
