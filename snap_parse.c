// The reader: source text to values
#include "snap_internal.h"
#include "snap_lex.h"

/* Parser */

void parse_raise(Snap* snap, const char* err, const char* format, ...) {
  char msg[256];
  va_list args;
  va_start(args, format);
  vsnprintf(msg, sizeof(msg), format, args);
  va_end(args);
  snap->cause = snap_err_new_format(snap, err, "%s", msg);
  longjmp(snap->jmp, 1);
}

static SValue parse_expr(Snap* snap, SnapLex* lex, int token);

static SValue parse_sexpr(Snap* snap, SnapLex* lex) {
  SCons* sexpr;
  SCons* tail;
  int token = snap_lex_next_token(lex);
  if (token == ')') {
    return create_empty();
  }
  sexpr = tail = (SCons*)anchor(snap, (SObject*)cons_new(snap));
  sexpr->first = parse_expr(snap, lex, token);
  // parse_expr() raises on EOF, so this always ends at ')'
  while ((token = snap_lex_next_token(lex)) != ')') {
    SCons* next = cons_new(snap);
    tail->rest = create_obj(next);
    tail = next;
    tail->first = parse_expr(snap, lex, token);
  }
  release(snap);
  return create_obj(sexpr);
}

static void parse_skip_seq(const int open, const int close, SnapLex* lex) {
  int count = 1;
  int token;
  do {
    token = snap_lex_next_token(lex);
    if (token == open) count++;
    if (token == close) count--;
  } while (count > 0 &&
           token != TK_EOF &&
           token != TK_TOO_BIG &&
           token != TK_INVALID);
}

static SValue parse_vec(Snap* snap, SnapLex* lex) {
  int token;
  SVec* vec = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, 0));
  // parse_expr() raises on EOF, so this always ends at ']'
  while ((token = snap_lex_next_token(lex)) != ']') {
    SValue val = parse_expr(snap, lex, token);
    vec_push(snap, vec, val);
  }
  release(snap);
  return create_obj(vec);
}

static SValue parse_map(Snap* snap, SnapLex* lex) {
  int token;
  SMap* map = (SMap*)anchor(snap, (SObject*)snap_map_new(snap));
  // parse_expr() raises on EOF, so this always ends at '}'
  while ((token = snap_lex_next_token(lex)) != '}') {
    SValue key = parse_expr(snap, lex, token), val;
    if (is_obj(key)) anchor(snap, as_obj(key));
    if ((token = snap_lex_next_token(lex)) == '}') {
      parse_raise(snap, "snap_parse_error", "Map literal needs a value for every key on line %d", lex->line);
    }
    val = parse_expr(snap, lex, token);
    snap_map_put(snap, map, key, val);
    if (is_obj(key)) release(snap);
  }
  release(snap);
  return create_obj(map);
}

// 'x, ~x and ~@x read as (quote x), (unquote x) and (splice x)
static SValue parse_wrap(Snap* snap, SnapLex* lex, int form) {
  SCons* sexpr = (SCons*)anchor(snap, (SObject*)cons_new(snap));
  sexpr->first = create_form(form);
  sexpr->rest = create_obj(cons_new(snap));
  as_cons(sexpr->rest)->first = parse_expr(snap, lex, snap_lex_next_token(lex));
  release(snap);
  return create_obj(sexpr);
}


// Whether expr is (form x)
static bool is_wrapped(SValue expr, int form) {
  return is_cons(expr) && !is_empty(expr) && is_form(as_cons(expr)->first) &&
         as_form(as_cons(expr)->first) == form;
}

static SValue quasi(Snap* snap, SValue expr);

// Code that makes a list of the quasiquoted elements: (list a b) or, if
// some are spliced in, (concat (list a) b). The functions themselves go in
// the code rather than their names, so locals can't hide them.
static SValue quasi_list(Snap* snap, SCons* elems) {
  SnapList code = { NULL, NULL };
  SCons* curr;
  SValue* slot;
  bool splice = false;
  for (curr = elems; curr; curr = as_cons(curr->rest)) {
    if (is_wrapped(curr->first, TK_SPLICE)) splice = true;
  }
  *list_add(snap, &code) = create_cfunc(splice ? builtin_concat : builtin_list);
  for (curr = elems; curr; curr = as_cons(curr->rest)) {
    slot = list_add(snap, &code);
    if (is_wrapped(curr->first, TK_SPLICE)) {
      *slot = list_nth(as_cons(curr->first), 1);
    } else if (splice) {
      SnapList part = { NULL, NULL };
      *list_add(snap, &part) = create_cfunc(builtin_list);
      *list_add(snap, &part) = create_nil();
      part.tail->first = quasi(snap, curr->first);
      *slot = list_done(snap, &part);
    } else {
      *slot = quasi(snap, curr->first);
    }
  }
  return list_done(snap, &code);
}

// Expands `expr into code that builds it, with ~x replaced by the value of x
// and ~@x by the elements of x. Symbols and special forms are quoted, and
// other atoms are their own value.
static SValue quasi(Snap* snap, SValue expr) {
  int i;
  SnapList code = { NULL, NULL };
  switch (snap_type(expr)) {
    case STYPE_CONS:
      if (is_empty(expr)) return expr;
      if (is_wrapped(expr, TK_UNQUOTE)) return list_nth(as_cons(expr), 1);
      if (is_wrapped(expr, TK_SPLICE)) {
        parse_raise(snap, "snap_parse_error", "~@ must be in a list or vector");
      }
      return quasi_list(snap, as_cons(expr));
    case STYPE_VEC: {
      SVec* vec = as_vec(expr);
      SVec* out;
      for (i = 0; i < vec->len; ++i) {
        if (is_wrapped(*snap_vec_at(vec, i), TK_SPLICE)) break;
      }
      if (i < vec->len) { // A splice: (vec (concat ...)) of the elements as a list
        SValue elems;
        for (i = 0; i < vec->len; ++i) *list_add(snap, &code) = *snap_vec_at(vec, i);
        elems = list_done(snap, &code);
        anchor(snap, as_obj(elems));
        code.head = NULL;
        *list_add(snap, &code) = create_cfunc(builtin_vec);
        *list_add(snap, &code) = create_nil();
        code.tail->first = quasi_list(snap, as_cons(elems));
        expr = list_done(snap, &code);
        release(snap);
        return expr;
      }
      out = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, vec->len));
      for (i = 0; i < vec->len; ++i) {
        SValue val = quasi(snap, *snap_vec_at(vec, i));
        vec_push(snap, out, val);
      }
      release(snap);
      return create_obj(out);
    }
    case STYPE_MAP: {
      SMap* map = as_map(expr);
      SMap* out = (SMap*)anchor(snap, (SObject*)snap_map_new(snap));
      for (i = 0; i < map->used; ++i) { // A literal has no deleted entries
        SValue key = quasi(snap, map->entries[2 * i]), val;
        if (is_obj(key)) anchor(snap, as_obj(key));
        val = quasi(snap, map->entries[2 * i + 1]);
        snap_map_put(snap, out, key, val);
        if (is_obj(key)) release(snap);
      }
      release(snap);
      return create_obj(out);
    }
    case STYPE_SYM:
    case STYPE_FORM:
      *list_add(snap, &code) = create_form(TK_QUOTE);
      *list_add(snap, &code) = expr;
      return list_done(snap, &code);
    default:
      return expr;
  }
}

static SValue parse_int(Snap* snap, SnapLex* lex) {
  char* end;
  long long i;
  errno = 0;
  i = strtoll(lex->val, &end, strchr(lex->val, 'x') ? 16 : 10);
  if (errno == ERANGE || i < SNAP_INT_MIN || i > SNAP_INT_MAX) {
    parse_raise(snap, "snap_parse_error", "Integer literal is out of range on line %d", lex->line);
  }
  return create_int(i);
}

// Replaces \n, \t, \r, \\ and \" in a string literal, in place
static void parse_escapes(Snap* snap, SnapLex* lex) {
  char* in, * out;
  for (in = out = lex->val; *in; ++in) {
    if (*in != '\\') {
      *out++ = *in;
      continue;
    }
    switch (*++in) {
      case 'n': *out++ = '\n'; break;
      case 't': *out++ = '\t'; break;
      case 'r': *out++ = '\r'; break;
      case '\\': case '"': *out++ = *in; break;
      default:
        parse_raise(snap, "snap_parse_error", "Unknown escape '\\%c' on line %d", *in, lex->line);
    }
  }
  *out = '\0';
}

// Counts a level of nesting in the reader or compiler toward SNAP_MAX_C_DEPTH.
// The caller takes it back off when it's done, or compile_forms() does after
// a raise.
void parse_enter(Snap* snap, const char* err) {
  if (++snap->c_depth > SNAP_MAX_C_DEPTH) parse_raise(snap, err, "Too deeply nested");
}

static SValue parse_value(Snap* snap, SnapLex* lex, int token);

static SValue parse_expr(Snap* snap, SnapLex* lex, int token) {
  SValue val;
  parse_enter(snap, "snap_parse_error");
  val = parse_value(snap, lex, token);
  snap->c_depth--;
  return val;
}

static SValue parse_value(Snap* snap, SnapLex* lex, int token) {
  switch (token) {
    case '(':
      return parse_sexpr(snap, lex);
    case '[':
      return parse_vec(snap, lex);
    case '{':
      return parse_map(snap, lex);
    case '\'':
      return parse_wrap(snap, lex, TK_QUOTE);
    case '~':
      return parse_wrap(snap, lex, TK_UNQUOTE);
    case TK_SPLICE:
      return parse_wrap(snap, lex, TK_SPLICE);
    case '`': {
      SValue expr = parse_expr(snap, lex, snap_lex_next_token(lex)), code;
      if (is_obj(expr)) anchor(snap, as_obj(expr));
      code = quasi(snap, expr);
      if (is_obj(expr)) release(snap);
      return code;
    }
    case TK_INT:
      return parse_int(snap, lex);
    case TK_FLOAT:
      return float_new(snap, atof(lex->val));
    case TK_STR:
      parse_escapes(snap, lex);
      return create_obj(snap_str_new(snap, lex->val));
    case TK_ID:
      return create_obj(snap_sym_new(snap, lex->val));
    case TK_KEY:
      return create_obj(snap_key_new(snap, lex->val));
    case TK_TRUE:
    case TK_FALSE:
      return create_bool(token == TK_TRUE);
    case TK_NIL:
      return create_nil();
    case TK_BEGIN:
    case TK_DEF:
    case TK_DEFMACRO:
    case TK_ELLIPSIS:
    case TK_IF:
    case TK_FN:
    case TK_LET:
    case TK_LOOP:
    case TK_QUOTE:
    case TK_RECUR:
    case TK_SET:
    case TK_PANIC:
    case TK_TRY:
    case TK_RECOVER:
    case TK_ADD: case TK_SUB: case TK_MUL: case TK_DIV: case TK_MOD:
    case TK_LT: case TK_LE: case TK_GT: case TK_GE: case TK_EQ:
    case TK_NOT: case TK_AND: case TK_OR:
      return create_form(token);
    case TK_EOF:
      parse_raise(snap, "snap_eof_error", "Premature end of file on line %d", lex->line);
      break;
    case TK_TOO_BIG:
      parse_raise(snap, "snap_parse_error", "Literal or identifier is too big on line %d", lex->line);
      break;
    case TK_INVALID:
    default:
      parse_raise(snap, "snap_parse_error", "Invalid token on line %d", lex->line);
      break;
  }
  return create_nil();
}

bool parse_form(Snap* snap, SnapLex* lex, SValue* result) {
  int token = snap_lex_next_token(lex);
  if (TK_EOF == token) return false;
  *result = parse_expr(snap, lex, token);
  return true;
}
