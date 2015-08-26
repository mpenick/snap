#include "snap.h"
#include "snap_hash.h"
#include "snap_lex.h"

#include <assert.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define GC_EVERY_NUM_BYTES 2 * 1024 * 1024

enum {
  WHITE,
  GRAY,
  BLACK
};

#define push(type, obj) (type*)snap_push(snap, (SObject*)obj)
#define push_sym(str) push(SSymStr, str)

static bool read(Snap* snap, SnapLex* lex, SValue* val);
static SValue* lookup(Snap* snap, SValue name);
static SValue exec(Snap* snap, SValue val);
static void parse(Snap* snap, SnapLex* lex);

static SnapCodeGen* code_gen_new();
static void code_gen_destroy(SnapCodeGen* code_gen);
static void insts_append(SnapList* insts, int opcode, int arg);

static void print_cons(SCons* cons);
static void print_hash(SHash* hash);

static int arity(SCons* cons) {
  int n = 0;
  for (; cons; cons = as_cons(cons->rest)) n++;
  return n;
}

static void push_val(Snap* snap, SValue val) {
  if (is_obj(val)) snap_push(snap, val.o);
}

static void pop_val(Snap* snap, SValue val) {
  if (is_obj(val)) {
    assert(snap->anchored[snap->anchored_top - 1] == val.o);
    snap_pop(snap);
  }
}

SValue create_undef() {
  SValue val;
  val.type = STYPE_UNDEF;
  val.o = NULL;
  return val;
}

SValue create_nil() {
  SValue val;
  val.type = STYPE_NIL;
  val.o = NULL;
  return val;
}

SValue create_empty() {
  SValue val;
  val.type = STYPE_CONS;
  val.o = NULL;
  return val;
}

SValue create_bool(bool b) {
  SValue val;
  val.type = STYPE_BOOL;
  val.b = b;
  return val;
}

SValue create_int(SnapInt i) {
  SValue val;
  val.type = STYPE_INT;
  val.i = i;
  return val;
}

SValue create_float(SnapFloat f) {
  SValue val;
  val.type = STYPE_FLOAT;
  val.f = f;
  return val;
}

SValue create_cfunc(SCFunc c) {
  SValue val;
  val.type = STYPE_CFUNC;
  val.c = c;
  return val;
}

SValue create_obj(SObject* o) {
  SValue val;
  val.type = o->type;
  val.o = o;
  return val;
}

static void gc_free(Snap* snap, SObject* obj) {
  switch (obj->type) {
    case STYPE_SYM:
    case STYPE_STR:
      snap->num_bytes_alloced -= (sizeof(SSymStr) + ((SSymStr*)obj)->len + 1);
      break;
    case STYPE_ERR:
      snap->num_bytes_alloced -= sizeof(SErr);
      break;
    case STYPE_CONS:
      snap->num_bytes_alloced -= sizeof(SCons);
      break;
    case STYPE_HASH:
      /* TODO: Track memory used by hash */
      snap->num_bytes_alloced -= sizeof(SHash);
      snap_hash_destroy(&((SHash*)obj)->table);
      break;
    case STYPE_SCOPE:
      snap->num_bytes_alloced -= sizeof(SScope);
      snap_hash_destroy(&((SScope*)obj)->vars);
      break;
    case STYPE_FN:
      snap->num_bytes_alloced -= sizeof(SFn);
      break;
    default:
      assert(0 && "Not a GC object");
      break;
  }
  free(obj);
}

static void gc_mark(Snap* snap, SObject* obj) {
  if (!obj) return;
  switch (obj->type) {
    case STYPE_SYM:
    case STYPE_STR:
      obj->mark = BLACK;
      break;
    case STYPE_ERR:
    case STYPE_CONS:
    case STYPE_HASH:
    case STYPE_SCOPE:
    case STYPE_FN:
      if (obj->mark == WHITE) {
        obj->mark = GRAY;
        obj->gc_next = snap->gray;
        snap->gray = obj;
      }
      break;
    default:
      assert(0 && "Not a GC object");
      break;
  }
}

static void gc_mark_val(Snap* snap, SValue val) {
  if (is_obj(val)) gc_mark(snap, val.o);
}

static void gc_mark_hash(Snap* snap, SnapHash* hash) {
  int i;
  for (i = 0; i < hash->capacity; ++i) {
    SEntry* entry = &hash->entries[i];
    if (!is_undef(entry->key)) {
      gc_mark_val(snap, entry->key);
      gc_mark_val(snap, entry->val);
    }
  }
}

static void gc_mark_children(Snap* snap, SObject* obj) {
  switch (obj->type) {
    case STYPE_ERR:
      gc_mark(snap, (SObject*)((SErr*)obj)->msg);
      gc_mark(snap, (SObject*)((SErr*)obj)->inner);
      break;
    case STYPE_CONS:
      gc_mark_val(snap, ((SCons*)obj)->first);
      gc_mark_val(snap, ((SCons*)obj)->rest);
      break;
    case STYPE_HASH:
      gc_mark_hash(snap, &((SHash*)obj)->table);
      break;
     case STYPE_SCOPE:
      gc_mark_hash(snap, &((SScope*)obj)->vars);
      gc_mark(snap, (SObject*)((SScope*)obj)->up);
      break;
    case STYPE_FN:
      gc_mark(snap, (SObject*)((SFn*)obj)->name);
      gc_mark(snap, (SObject*)((SFn*)obj)->scope);
      gc_mark(snap, (SObject*)((SFn*)obj)->params);
      gc_mark(snap, (SObject*)((SFn*)obj)->body);
      break;
    default:
      assert(0 && "Not a valid gray object");
      break;
  }
}

static void gc_collect(Snap* snap) {
  int i;
  SnapFrame* frame;
  SObject** obj;

  frame = snap->frame;
  while (frame) {
    gc_mark(snap, (SObject*)frame->scope);
    frame = frame->up;
  }

  for (i = 0; i < snap->anchored_top; ++i) {
    gc_mark(snap, snap->anchored[i]);
  }

  gc_mark_hash(snap, &snap->globals);
  gc_mark(snap, (SObject*)snap->cause);

  while (snap->gray) {
    SObject* temp = snap->gray;
    snap->gray = snap->gray->gc_next;
    gc_mark_children(snap, temp);
  }

  obj = &snap->all;

  while (*obj) {
    if ((*obj)->mark == WHITE) {
      SObject* temp = *obj;
      *obj = (*obj)->next;
      gc_free(snap, temp);
    } else {
      (*obj)->mark = WHITE;
      obj = &(*obj)->next;
    }
  }
}

static SObject* gc_new(Snap* snap, uint8_t type, size_t size) {
  assert(size > sizeof(SObject));
  if (snap->num_bytes_alloced_last_gc > GC_EVERY_NUM_BYTES) {
    gc_collect(snap);
    snap->num_bytes_alloced_last_gc = 0;
  }
  SObject* obj = (SObject*)malloc(size);
  obj->type = type;
  obj->mark = WHITE;
  obj->gc_next = NULL;
  obj->next = snap->all;
  snap->all = obj;
  snap->num_bytes_alloced += size;
  snap->num_bytes_alloced_last_gc += size;
  return obj;
}

SSymStr* snap_str_new(Snap* snap, const char* str) {
  size_t len = strlen(str);
  SSymStr* s = (SSymStr*)gc_new(snap, STYPE_STR, sizeof(SSymStr) + len + 1);
  s->len = len;
  strcpy(s->data, str);
  return s;
}

SSymStr* snap_sym_new(Snap* snap, const char* sym) {
  size_t len = strlen(sym);
  SSymStr* s = (SSymStr*)gc_new(snap, STYPE_SYM, sizeof(SSymStr) + len + 1);
  s->len = len;
  strcpy(s->data, sym);
  return s;
}

SErr* snap_err_new(Snap* snap, int code, const char* msg) {
  SErr* e = (SErr*)gc_new(snap, STYPE_ERR, sizeof(SErr));
  e->code = code;
  e->msg = NULL;
  e->inner = snap->cause;
  snap_push(snap, (SObject*)e);
  e->msg = snap_str_new(snap, msg);
  snap_pop(snap);
  return e;
}

SCons* snap_cons_new(Snap* snap) {
 SCons* c = (SCons*)gc_new(snap,  STYPE_CONS, sizeof(SCons));
 c->first = create_nil();
 c->rest = create_empty();
 return c;
}

SHash* snap_hash_new(Snap* snap) {
  SHash* h = (SHash*)gc_new(snap, STYPE_HASH, sizeof(SHash));
  snap_hash_init(&h->table);
  return h;
}

SScope* snap_scope_new(Snap* snap) {
  SScope* s = (SScope*)gc_new(snap, STYPE_SCOPE, sizeof(SScope));
  snap_hash_init(&s->vars);
  s->up = NULL;
  return s;
}

SFn* snap_fn_new(Snap* snap, int n, SCons* params, SCons* body) {
  SFn* fn = (SFn*)gc_new(snap, STYPE_FN, sizeof(SFn));
  fn->n = n;
  fn->name = NULL;
  fn->scope = snap->frame->scope;
  fn->params = params;
  fn->body = body;
  return fn;
}


void snap_def(Snap* snap, const char* name, SValue val) {
  SValue key = create_obj((SObject*)snap_sym_new(snap, name));
  snap_hash_put(snap->frame->scope ? &snap->frame->scope->vars : &snap->globals, key, val);
}

void snap_def_cfunc(Snap* snap, const char* name, SCFunc cfunc) {
  snap_def(snap, name, create_cfunc(cfunc));
}

void snap_throw(Snap* snap, int code, const char* format, ...) {
  char msg[256];
  va_list args;
  va_start(args, format);
  vsnprintf(msg, sizeof(msg), format, args);
  va_end(args);
  snap->cause = snap_err_new(snap, code, msg);
  longjmp(snap->trying->buf, 1);
}

SValue snap_exec(Snap* snap, const char* expr) {
  SValue res;
  SValue val;
  SnapTry trying;
  SnapLex lex;
  lex.buf = expr;
  lex.buf_size = strlen(lex.buf);
  lex.p = lex.buf;
  lex.line = 0;
  trying.up = NULL;
  snap->trying = &trying;
  if (!setjmp(trying.buf)) {
    while (read(snap, &lex, &val)) {
      push_val(snap, val);
      res = exec(snap, val);
      pop_val(snap, val);
    }
  } else {
    fprintf(stderr, "Error '%s' (code: %d)\n",
            snap->cause->msg->data,
            snap->cause->code);
    res = create_obj((SObject*)snap->cause);
  }
  return res;
}

void snap_parse(Snap* snap, const char* expr) {
  SnapLex lex;
  lex.buf = expr;
  lex.buf_size = strlen(lex.buf);
  lex.p = lex.buf;
  lex.line = 0;
  parse(snap, &lex);
}

SObject* snap_push(Snap* snap, SObject* obj) {
  if (snap->anchored_top >= snap->anchored_capacity) {
    snap->anchored_capacity *= 2;
    snap->anchored
        = (SObject**)realloc(snap->anchored,
                             snap->anchored_capacity * sizeof(SObject*));
  }
  snap->anchored[snap->anchored_top++] = obj;
  return obj;
}

void snap_pop(Snap* snap) {
  assert(snap->anchored_top > 0);
  snap->anchored_top--;
}

static bool is_tail(SnapLex* lex, int token) {
  int count = (token == '(' ? 1 : 0);
  const char* p = lex->p;
  do {
    token = snap_lex_next_token(lex);
    if (token == '(') count++;
    if (token == ')') count--;
  } while (count > 0);
  lex->p = p;
  return token == ')';
}

enum {
  LOAD_GLOBAL,
  STORE_GLOBAL,
  LOAD_LOCAL,
  STORE_LOCAL,
  LOAD_CONSTANT,
};

static bool is_global_scope(Snap* snap) {
  return snap->code_gen->up == NULL;
}

static void parse_expr(Snap* snap, SnapLex* lex, int token);

static void parse(Snap* snap, SnapLex* lex) {
  snap->code_gen = code_gen_new();
  parse_expr(snap, lex, snap_lex_next_token(lex));
}

static int get_or_create_index(SnapHash* hash, SValue key) {
  int index;
  SValue* val = snap_hash_get(hash, key);
  if (val) {
    index = val->i;
  } else {
    index = hash->count;
    snap_hash_put(hash, key, create_int(index));
  }
  return index;
}

static void load_or_store_var(Snap* snap, SSymStr* sym, bool is_load) {
  SnapCodeGen* code_gen = snap->code_gen;
  SnapScope* scope = code_gen->scope;
  SValue key = create_obj((SObject*)sym);
  int index;

  while (scope) {
    SValue* val = snap_hash_get(&scope->local_names, key);
    if (val) {
      insts_append(&code_gen->insts, is_load ? LOAD_LOCAL : STORE_LOCAL, val->i);
      return;
    }
    scope = scope->up;
  }

  index = get_or_create_index(&code_gen->global_names, key);
  insts_append(&code_gen->insts, is_load ? LOAD_GLOBAL : STORE_GLOBAL, index);
}

static void load_constant(Snap* snap, SValue constant) {
  SnapCodeGen* code_gen = snap->code_gen;
  int index = get_or_create_index(&code_gen->constants, constant);
  insts_append(&code_gen->insts, LOAD_CONSTANT, index);
}

static void parse_def(Snap* snap, SnapLex* lex) {
  SSymStr* sym;
  int token = snap_lex_next_token(lex);
  if (token != TK_ID) {
    snap_throw(snap, 0, "Expected id for first parameter of define at %d", lex->line);
  }
  sym = push_sym(snap_sym_new(snap, lex->val));
  parse_expr(snap, lex, snap_lex_next_token(lex));
  load_or_store_var(snap, sym, false);
  snap_pop(snap);
  token = snap_lex_next_token(lex);
  if (token != ')') {
    snap_throw(snap, 0, "Expected ')' to terminate s-expression at %d", lex->line);
  }
}

static void parse_sexpr(Snap* snap, SnapLex* lex) {
  int token = snap_lex_next_token(lex);

  switch (token) {
    case ')':
      break;
    case TK_ID:
      break;
    case TK_DO:
      break;
    case TK_DEF:
      parse_def(snap, lex);
      break;
    case TK_ELLIPSIS:
      break;
    case TK_IF:
      break;
    case TK_FN:
      break;
    case TK_LET:
      break;
    case TK_QUOTE:
      break;
    case TK_RECUR:
      break;
    case TK_SET:
      break;
    case TK_THROW:
      break;
    case TK_TRY:
      break;
    default:
      snap_throw(snap, 0, "Expected id, special form or expr at %d", lex->line);
      break;
  }
}

static void parse_expr(Snap* snap, SnapLex* lex, int token) {
  switch (token) {
    case '(':
      parse_sexpr(snap, lex);
      break;
    case '\'':
      break;
    case TK_INT:
      load_constant(snap, create_int(atol(lex->val)));
      break;
    case TK_FLOAT:
      load_constant(snap, create_float(atof(lex->val)));
      break;
    case TK_STR:
      load_constant(snap, create_obj((SObject*)snap_str_new(snap, lex->val)));
      break;
    case TK_ID:
      break;
    case TK_TRUE:
    case TK_FALSE:
      load_constant(snap, create_bool(token == TK_TRUE));
      break;
    case TK_NIL:
      break;
    case TK_DO:
    case TK_DEF:
    case TK_ELLIPSIS:
    case TK_IF:
    case TK_FN:
    case TK_LET:
    case TK_QUOTE:
    case TK_RECUR:
    case TK_SET:
    case TK_THROW:
    case TK_TRY:
      // error
      break;
    case TK_EOF:
      break;
    default:
      snap_throw(snap, 0, "Invalid token on line %d", lex->line);
      break;
  }
}

static bool read_val(Snap* snap, SnapLex* lex, int token, SValue* val);

static SCons* read_cons(Snap* snap, SnapLex* lex, int term) {
  int token = snap_lex_next_token(lex);
  SCons* first = NULL;
  SCons** cons = &first;
  while (token != term && token != TK_EOF) {
    SValue val;
    *cons = first ? snap_cons_new(snap)
                  : (SCons*)snap_push(snap, (SObject*)snap_cons_new(snap));
    if (!read_val(snap, lex, token, &val)) {
      snap_throw(snap, 1, "Premature EOF at %d", lex->line);
    }
    (*cons)->first = val;
    cons = (SCons**)&(*cons)->rest.o;
    token = snap_lex_next_token(lex);
  }
  if (token != term) {
    snap_throw(snap, 1, "Expected '%c' at line %d", term, lex->line);
  }
  if (first) snap_pop(snap);
  return first;
}

static SValue read_coll(Snap* snap, SnapLex* lex,
                        const char* make, int term) {
  SCons* coll = (SCons*)snap_push(snap, (SObject*)snap_cons_new(snap));
  SCons* cons;
  coll->first = create_obj((SObject*)snap_sym_new(snap, make));
  cons = read_cons(snap, lex, term);
  if (cons) {
    snap_push(snap, (SObject*)cons);
    coll->rest = create_obj((SObject*)snap_cons_new(snap));
    as_cons(coll->rest)->first
        = create_obj((SObject*)snap_cons_new(snap));
    as_cons(as_cons(coll->rest)->first)->first.type = STYPE_FORM;
    as_cons(as_cons(coll->rest)->first)->first.i = TK_QUOTE;
    as_cons(as_cons(coll->rest)->first)->rest
        = create_obj((SObject*)snap_cons_new(snap));
    as_cons(as_cons(as_cons(coll->rest)->first)->rest)->first
        = create_obj((SObject*)cons);
    snap_pop(snap);
  }
  snap_pop(snap);
  return create_obj((SObject*)coll);
}

static bool read_quote(Snap* snap, SnapLex* lex, SValue* val) {
  bool res;
  SCons* cons = (SCons*)snap_push(snap, (SObject*)snap_cons_new(snap));
  cons->first.type = STYPE_FORM;
  cons->first.i = TK_QUOTE;
  cons->rest = create_obj((SObject*)snap_cons_new(snap));
  *val = create_obj((SObject*)cons);
  res = read_val(snap, lex,
                  snap_lex_next_token(lex), &as_cons(cons->rest)->first);
  snap_pop(snap);
  return res;
}

static bool read_val(Snap* snap, SnapLex* lex, int token, SValue* val) {
  switch(token) {
    case '(':
      val->type = STYPE_CONS;
      val->o = (SObject*)read_cons(snap, lex, ')');
      break;
    case '{':
      *val = read_coll(snap, lex, "make-hash", '}');
      break;
    case '\'':
      return read_quote(snap, lex, val);
    case TK_INT:
      *val = create_int(atol(lex->val));
      break;
    case TK_FLOAT:
      *val = create_float(atof(lex->val));
      break;
    case TK_STR:
      *val = create_obj((SObject*)snap_str_new(snap, lex->val));
      break;
    case TK_ID:
      *val = create_obj((SObject*)snap_sym_new(snap, lex->val));
      break;
    case TK_TRUE:
    case TK_FALSE:
      *val = create_bool(token == TK_TRUE);
      break;
    case TK_NIL:
      *val = create_nil();
      break;
    case TK_DO:
    case TK_DEF:
    case TK_ELLIPSIS:
    case TK_IF:
    case TK_FN:
    case TK_LET:
    case TK_QUOTE:
    case TK_RECUR:
    case TK_SET:
    case TK_THROW:
    case TK_TRY:
      val->type = STYPE_FORM;
      val->i = token;
      break;
    case TK_EOF:
      return false;
    default:
      snap_throw(snap, 0, "Invalid token on line %d", lex->line);
      break;
  }

  return true;
}

static bool read(Snap* snap, SnapLex* lex, SValue* val) {
  return read_val(snap, lex, snap_lex_next_token(lex), val);
}

static SValue* lookup(Snap* snap, SValue name) {
  SValue* res;
  SScope* scope = snap->frame->scope;
  while (scope) {
    res = snap_hash_get(&scope->vars, name);
    if (res) return res;
    scope = scope->up;
  }
  return snap_hash_get(&snap->globals, name);
}

static SValue lookup_sym(Snap* snap, SValue sym) {
  SValue* val = lookup(snap, sym);
  if (val) return *val;
  snap_throw(snap, 0, "Unable to find symbol %s", as_sym(sym)->data);
  return create_nil();
}

static bool is_recur(SCons* cons) {
  return is_cons(cons->first) &&
      as_cons(cons->first) &&
      is_form(as_cons(cons->first)->first) &&
      as_cons(cons->first)->first.i == TK_RECUR;
}

static SValue exec_cons(Snap* snap, SCons* cons);
static SValue exec_fn(Snap* snap, SFn* fn, SCons* args);

static SValue exec_body(Snap* snap, SCons* body) {
  SValue res;
  for (; body; body = as_cons(body->rest)) {
    if (is_recur(body)) {
      if (as_cons(body->rest)) {
        snap_throw(snap, 0, "Recur not in tail position");
      }
      snap->tail = body;
    } else {
      res = exec(snap, body->first);
    }
  }
  return res;
}

static SValue form_def(Snap* snap, SCons* args) {
  SValue res;
  if (!args ||
      !is_sym(args->first) ||
      !is_cons(args->rest)) {
    snap_throw(snap, 0, "Invalid def");
  }
  res = exec(snap, as_cons(args->rest)->first);
  if (is_fn(res)) {
    as_fn(res)->name = as_sym(args->first);
  }
  snap_def(snap,  as_sym(args->first)->data, res);
  return create_nil();
}

static SValue form_if(Snap* snap, SCons* args) {
  SValue res;
  SValue cond;
  SCons* body;
  if (!args ||
      !is_cons(args->rest) || !as_cons(args->rest) ||
      !is_cons(as_cons(args->rest)->rest)) {
    snap_throw(snap, 0, "Invalid if");
  }
  body = as_cons(args->rest);
  cond = exec(snap, args->first);
  if (is_nil(cond) || (is_bool(cond) && !cond.b)) {
    body = as_cons(body->rest);
  }
  if (is_recur(body)) {
    snap->tail = body;
  } else {
    res = exec(snap, body->first);
  }
  return res;
}

static SValue form_fn(Snap* snap, SCons* args) {
  int n = 0;
  bool is_vararg = false;
  SCons* param;
  if (!args ||
      !is_cons(args->first) ||
      !is_cons(args->rest)) {
    snap_throw(snap, 0, "Invalid fn");
  }
  for (param = as_cons(args->first); param; param = as_cons(param->rest)) {
    if (!as_cons(param->rest) &&
        param->first.type == STYPE_FORM &&
        param->first.i == TK_ELLIPSIS) {
      is_vararg = true;
    } else if (param->first.type == STYPE_SYM) {
      n++;
    } else {
      snap_throw(snap, 0, "Fn parameter is invalid");
    }
  }
  return create_obj((SObject*)snap_fn_new(snap,
                                          is_vararg ? -n : n,
                                          as_cons(args->first),
                                          as_cons(args->rest)));
}

static SValue form_let(Snap* snap, SCons* args) {
  SValue res;
  SCons* arg;
  SScope* scope = snap_scope_new(snap);
  scope->up = snap->frame->scope;
  snap->frame->scope = scope;
  if (!args ||
      !is_cons(args->first) ||
      !is_cons(args->rest)) {
    snap_throw(snap, 0, "Invalid let");
  }
  for (arg = as_cons(args->first); arg; arg = as_cons(arg->rest)) {
    if (!is_cons(arg->first) ||
        !is_sym(as_cons(arg->first)->first) ||
        !is_cons(as_cons(arg->first)->rest)) {
      snap_throw(snap, 0, "Invalid let binding");
    }
    snap_hash_put(&scope->vars,
                  as_cons(arg->first)->first,
                  exec(snap, as_cons(as_cons(arg->first)->rest)->first));
  }
  res = exec_body(snap, as_cons(args->rest));
  snap->frame->scope = snap->frame->scope->up;
  return res;
}

static SValue form_throw(Snap* snap, SCons* args) {
  SValue res;
  SErr* err;
  if (!args ||
      !is_cons(args->rest) || !as_cons(args->rest)) {
    snap_throw(snap, 0, "Invalid throw");
  }
  err = (SErr*)snap_push(snap, gc_new(snap, STYPE_ERR, sizeof(SErr)));
  res = exec(snap, args->first);
  if (!is_int(res)) {
    snap_throw(snap, 0, "Expected an int for first argument to throw");
  }
  err->code = res.i;
  res = exec(snap, as_cons(args->rest)->first);
  if (!is_str(res)) {
    snap_throw(snap, 0, "Expected a str for second argument to throw");
  }
  err->msg = as_str(res);
  err->inner = snap->cause;
  snap->cause = err;
  snap_pop(snap);
  longjmp(snap->trying->buf, 1);
  return create_nil(); /* Never happens */
}

static SValue form_try(Snap* snap, SCons* args) {
  SValue res;
  SnapTry trying;
  SnapFrame* prev_frame;
  size_t prev_anchored_top;
  if (!args ||
      !is_cons(args->rest) || !as_cons(args->rest) ||
      !is_cons(as_cons(args->rest)->first)) {
    snap_throw(snap, 0, "Invalid try");
  }
  prev_frame = snap->frame;
  prev_anchored_top = snap->anchored_top;
  trying.up = snap->trying;
  snap->trying = &trying;
  if(!setjmp(trying.buf)) {
    res = exec(snap, args->first);
    snap->trying = trying.up;
  } else {
    SValue handler;
    SCons* cons;
    snap->frame = prev_frame;
    snap->anchored_top = prev_anchored_top;
    snap->trying = trying.up;
    handler = exec(snap, as_cons(args->rest)->first);
    push_val(snap, handler);
    cons = (SCons*)snap_push(snap, (SObject*)snap_cons_new(snap));
    cons->first = create_obj((SObject*)snap->cause);
    snap->cause = snap->cause->inner;
    switch (handler.type) {
      case STYPE_CFUNC:
        res = handler.c(snap, cons);
        break;
      case STYPE_FN:
        res = exec_fn(snap, as_fn(handler), cons);
        break;
      default:
        snap_throw(snap, 0, "Try handler is not a C function or fn");
        break;
    }
    snap_pop(snap);
    pop_val(snap, handler);
  }
  return res;
}

static SValue form_set(Snap* snap, SCons* args) {
  SValue* val;
  if (!args ||
      !is_sym(args->first) ||
      !is_cons(args->rest) || !as_cons(args->rest)) {
    snap_throw(snap, 0, "Invalid set!");
  }
  val = lookup(snap, args->first);
  if (!val) {
    snap_throw(snap, 0, "Variable '%s' is not defined",
               as_sym(args->first)->data);
  }
  *val = exec(snap, as_cons(args->rest)->first);
  return create_nil();
}

static SValue exec_cfunc(Snap* snap, SCFunc cfunc, SCons* args) {
  SValue res;
  SCons* arg = args;
  SCons* first = NULL;
  SCons** cons = &first;
  for (; arg; arg = as_cons(arg->rest)) {
    *cons = first ? snap_cons_new(snap)
                  : (SCons*)snap_push(snap, (SObject*)snap_cons_new(snap));
    (*cons)->first = exec(snap, arg->first);
    cons = (SCons**)&(*cons)->rest.o;
  }
  res = cfunc(snap, first);
  if (first) snap_pop(snap);
  return res;
}

void bind_param(Snap* snap, SScope* scope, SValue sym, SValue val) {
  if (snap_hash_put(&scope->vars, sym, val)) {
    snap_throw(snap, 0, "Duplicate parameter %s", as_sym(sym)->data);
  }
}

bool bind_params(Snap* snap, SFn* fn, SCons* args) {
  SCons* param;
  int n = arity(args);
  SScope* new_scope = (SScope*)snap_push(snap,
                                         (SObject*)snap_scope_new(snap));
  new_scope->up = fn->scope;
  if(fn->n >= 0) {
    if(n != fn->n) return false;
    for (param = fn->params; param; param = as_cons(param->rest)) {
      bind_param(snap, new_scope,
                 param->first, exec(snap, args->first));
      args = as_cons(args->rest);
    }
  } else {
    SCons* first = NULL;
    SCons** cons = &first;
    int p = -fn->n;
    if (n < p) return false;
    for (param = fn->params; param && p > 1; param = as_cons(param->rest)) {
      bind_param(snap, new_scope,
                 param->first, exec(snap, args->first));
      args = as_cons(args->rest);
      p--;
    }
    for (; args; args = as_cons(args->rest)) {
      *cons = first ? snap_cons_new(snap)
                    : (SCons*)snap_push(snap, (SObject*)snap_cons_new(snap));
      (*cons)->first = exec(snap, args->first);
      cons = (SCons**)&(*cons)->rest.o;
    }
    bind_param(snap, new_scope,
               param->first,
               create_obj((SObject*)first));
    snap_pop(snap);
  }
  snap->frame->scope = new_scope;
  snap_pop(snap);
  return true;
}

static SValue exec_fn(Snap* snap, SFn* fn, SCons* args) {
  SValue res;
  SnapFrame frame;
  frame.scope = snap->frame->scope;
  frame.up = snap->frame;
  snap->frame = &frame;
  /* Bind params in current scope into the new scope */
  if (!bind_params(snap, fn, args)) {
    snap_throw(snap, 0, "Invalid number of arguments");
  }
  for (;;) {
    snap->tail = NULL;
    res = exec_body(snap, fn->body);
    if (!snap->tail) break;
    if (!bind_params(snap, fn,
                     as_cons(as_cons(snap->tail->first)->rest))) {
      snap_throw(snap, 0, "Invalid number of arguments (recur)");
    }
  }
  snap->frame = snap->frame->up;
  return res;
}

static SValue exec_form(Snap* snap, int form, SCons* args) {
  switch (form) {
    case TK_DO:
      return exec_body(snap, args);
    case TK_DEF:
      return form_def(snap, args);
    case TK_IF:
      return form_if(snap, args);
    case TK_FN:
      return form_fn(snap, args);
    case TK_LET:
      return form_let(snap, args);
    case TK_QUOTE:
      if (!args) {
        snap_throw(snap, 0, "Invalid quote");
      }
      return args->first;
    case TK_SET:
      return form_set(snap, args);
    case TK_THROW:
      return form_throw(snap, args);
    case TK_TRY:
      return form_try(snap, args);
    default:
      snap_throw(snap, 0, "Invalid primitive form");
      return create_nil();
  }
}

static SValue exec_cons(Snap* snap, SCons* cons) {
  SValue res;
  SValue first = exec(snap, cons->first);
  push_val(snap, first);
  switch (first.type) {
    case STYPE_FORM:
      res = exec_form(snap, first.i, as_cons(cons->rest));
      break;
    case STYPE_CFUNC:
      res = exec_cfunc(snap, first.c, as_cons(cons->rest));
      break;
    case STYPE_FN:
      res = exec_fn(snap, as_fn(first), as_cons(cons->rest));
      break;
    default:
      snap_throw(snap, 0, "Expected special form, C func or fn");
  }
  pop_val(snap, first);
  return res;
}

static SValue exec(Snap* snap, SValue val) {
  switch (val.type) {
    case STYPE_NIL:
    case STYPE_BOOL:
    case STYPE_INT:
    case STYPE_FLOAT:
    case STYPE_CFUNC:
    case STYPE_FORM:
    case STYPE_STR:
    case STYPE_ERR:
    case STYPE_HASH:
    case STYPE_FN:
      return val;
    case STYPE_SYM:
      return lookup_sym(snap, val);
    case STYPE_CONS:
      return as_cons(val) ? exec_cons(snap, as_cons(val)) : val;
    default:
      snap_throw(snap, 0, "Invalid type");
      return create_nil();
  }
}

void snap_print(SValue val) {
  switch(val.type) {
    case STYPE_NIL:
      printf("nil");
      break;
    case STYPE_BOOL:
      printf("%s", val.b ? "true" : "false");
      break;
    case STYPE_INT:
      printf("%ld", val.i);
      break;
    case STYPE_FLOAT:
      printf("%f", val.f);
      break;
    case STYPE_CFUNC:
      printf("<cfunc> %p", val.c);
      break;
    case STYPE_FORM:
      switch (val.i) {
        case TK_DO: printf("do"); break;
        case TK_DEF: printf("def"); break;
        case TK_ELLIPSIS: printf("..."); break;
        case TK_IF: printf("if"); break;
        case TK_FN: printf("fn"); break;
        case TK_LET: printf("let"); break;
        case TK_QUOTE: printf("quote"); break;
        case TK_RECUR: printf("recur"); break;
        case TK_SET: printf("set"); break;
        case TK_THROW: printf("throw"); break;
        case TK_TRY: printf("try"); break;
      }
      break;
    case STYPE_STR:
      printf("\"%s\"", as_str(val)->data);
      break;
    case STYPE_SYM:
      printf("%s", as_sym(val)->data);
      break;
    case STYPE_ERR:
      printf("<err> %d '%s'", as_err(val)->code, as_err(val)->msg->data);
      break;
    case STYPE_FN:
      printf("<fn> %s(%p)",
             as_fn(val)->name != NULL ? as_fn(val)->name->data : "",
             val.o);
      break;
    case STYPE_HASH:
      print_hash(as_hash(val));
      break;
    case STYPE_CONS:
      printf("(");
      print_cons(as_cons(val));
      printf(")");
      break;
  }
}

static void print_cons(SCons* cons) {
  int space = 0;
  while (cons != NULL) {
    if (space) printf(" ");
    space = 1;
    snap_print(cons->first);
    if (is_cons(cons->rest)) {
      cons = as_cons(cons->rest);
    } else {
      printf(" . ");
      snap_print(cons->rest);
      break;
    }
  }
}

static void print_hash(SHash* hash) {
  int i;
  SnapHash* table = &hash->table;
  int space = 0;
  printf("{");
  for (i = 0; i < table->capacity; ++i) {
    SEntry* entry = &table->entries[i];
    if (!is_undef(entry->key)) {
      if (space) printf(" ");
      space = 1;
      snap_print(entry->key);
      printf(" ");
      snap_print(entry->val);
    }
  }
  printf("}");
}

static SValue builtin_print(Snap* snap, SCons* args) {
  SCons* arg;
  int space = 0;
  for (arg = args; arg; arg = as_cons(arg->rest)) {
    if (space) printf(" ");
    space = 1;
    snap_print(arg->first);
  }
  printf("\n");
  return create_nil();
}

static SValue builtin_apply(Snap* snap, SCons* args) {
  SValue res;
  SCons* cons;
  if (!args ||
      !is_cons(args->rest) || !as_cons(args->rest) ||
      !is_cons(as_cons(args->rest)->first)) {
    snap_throw(snap, 0, "Invalid apply");
  }
  cons = (SCons*)snap_push(snap, (SObject*)snap_cons_new(snap));
  cons->first = args->first;
  cons->rest = as_cons(args->rest)->first;
  res = exec_cons(snap, cons);
  snap_pop(snap);
  return res;
}

static SValue builtin_isnil(Snap* snap, SCons* args) {
  if (!args) {
    snap_throw(snap, 0, "Invalid nil?");
  }
  return create_bool(is_nil(args->first));
}

static SValue builtin_isempty(Snap* snap, SCons* args) {
  if (!args) {
    snap_throw(snap, 0, "Invalid empty?");
  }
  return create_bool(is_cons(args->first) && !as_cons(args->first));
}

static SValue builtin_gc(Snap* snap, SCons* args) {
  gc_collect(snap);
  return create_nil();
}

static SValue builtin_cons(Snap* snap, SCons* args) {
  SCons* res;
  if (!args ||
      !is_cons(args->rest) || !as_cons(args->rest)) {
    snap_throw(snap, 0, "Invalid cons");
  }
  res = snap_cons_new(snap);
  res->first = args->first;
  res->rest = as_cons(args->rest)->first;
  return create_obj((SObject*)res);
}

static SValue builtin_first(Snap* snap, SCons* args) {
  if (!args || !as_cons(args->first)) {
    snap_throw(snap, 0, "Invalid first");
  }
  return as_cons(args->first)->first;
}

static SValue builtin_rest(Snap* snap, SCons* args) {
  if (!args || !as_cons(args->first)) {
    snap_throw(snap, 0, "Invalid rest");
  }
  return as_cons(args->first)->rest;
}

static SValue builtin_make_hash(Snap* snap, SCons* args) {
  SHash* hash = (SHash*)snap_push(snap, (SObject*)snap_hash_new(snap));
  SCons* cons;
  if (!args || !is_cons(args->first)) goto done;
  cons = as_cons(args->first);
  while (cons) {
    SValue key;
    if (!is_cons(cons->rest) || !as_cons(cons->rest)) {
      snap_throw(snap, 0, "Hash requires pairs");
    }
    key = exec(snap, cons->first);
    push_val(snap, key);
    if (!is_str(key)) {
      snap_throw(snap, 0, "Hash requires string keys");
    }
    snap_hash_put(&hash->table,
                  key,
                  exec(snap, as_cons(cons->rest)->first));
    cons = as_cons(as_cons(cons->rest)->rest);
    pop_val(snap, key);
  }
done:
  snap_pop(snap);
  return create_obj((SObject*)hash);
}

static SValue builtin_get(Snap* snap, SCons* args) {
  SHash* hash;
  SValue* val;
  if (!args || !is_hash(args->first) ||
      !is_cons(args->rest) || !as_cons(args->rest) ||
      !is_str(as_cons(args->rest)->first)) {
    snap_throw(snap, 0, "Invalid get");
  }
  hash = as_hash(args->first);
  val = snap_hash_get(&hash->table,
                      as_cons(args->rest)->first);
  if (!val) {
      if (is_cons(as_cons(args->rest)->rest) &&
          as_cons(as_cons(args->rest)->rest)) {
        return as_cons(as_cons(args->rest)->rest)->first;
      }
      return create_nil();
  }
  return *val;
}

static SValue builtin_put(Snap* snap, SCons* args) {
  SHash* hash;
  if (!args || !is_hash(args->first) ||
      !is_cons(args->rest) || !as_cons(args->rest) ||
      !is_str(as_cons(args->rest)->first) ||
      !is_cons(as_cons(args->rest)->rest) ||
      !as_cons(as_cons(args->rest)->rest)) {
    snap_throw(snap, 0, "Invalid put!");
  }
  hash = as_hash(args->first);
  return create_bool(snap_hash_put(&hash->table,
                                   as_cons(args->rest)->first,
                                   as_cons(as_cons(args->rest)->rest)->first));
}

static SValue builtin_del(Snap* snap, SCons* args) {
  SHash* hash;
  if (!args || !is_hash(args->first) ||
      !is_cons(args->rest) || !as_cons(args->rest) ||
      !is_str(as_cons(args->rest)->first)) {
    snap_throw(snap, 0, "Invalid del!");
  }
  hash = as_hash(args->first);
  return create_bool(snap_hash_delete(&hash->table,
                                      as_cons(args->rest)->first));
}

#define builtin_binop(name, iop, fop) \
static SValue builtin_##name(Snap* snap, SCons* args) { \
  SCons* a = args; \
  SCons* b; \
  if (!args || !is_cons(args->rest)) { \
    snap_throw(snap, 0, \
      "Binary function requires two parameters"); \
    return create_nil(); \
  } \
  b = as_cons(args->rest); \
  if (is_int(a->first) && is_int(b->first)) { \
    return iop(a, b); \
  } else if (is_float(a->first) && is_float(b->first)) { \
    return fop(a, b); \
  } else { \
    snap_throw(snap, 0, "Incompatible types"); \
    return create_nil(); \
  } \
}

#define lt_iop(a, b) create_bool(a->first.i < b->first.i)
#define lt_fop(a, b) create_bool(a->first.f < b->first.f)
builtin_binop(lt, lt_iop, lt_fop)

#define gt_iop(a, b) create_bool(a->first.i > b->first.i)
#define gt_fop(a, b) create_bool(a->first.f > b->first.f)
builtin_binop(gt, gt_iop, gt_fop)

#define le_iop(a, b) create_bool(a->first.i <= b->first.i)
#define le_fop(a, b) create_bool(a->first.f <= b->first.f)
builtin_binop(le, le_iop, le_fop)

#define ge_iop(a, b) create_bool(a->first.i >= b->first.i)
#define ge_fop(a, b) create_bool(a->first.f >= b->first.f)
builtin_binop(ge, ge_iop, ge_fop)

#define eq_iop(a, b) create_bool(a->first.i == b->first.i)
#define eq_fop(a, b) create_bool(a->first.f == b->first.f)
builtin_binop(eq, eq_iop, eq_fop)

#define add_iop(a, b) create_int(a->first.i + b->first.i)
#define add_fop(a, b) create_float(a->first.f + b->first.f)
builtin_binop(add, add_iop, add_fop)

#define sub_iop(a, b) create_int(a->first.i - b->first.i)
#define sub_fop(a, b) create_float(a->first.f - b->first.f)
builtin_binop(sub, sub_iop, sub_fop)

#define mul_iop(a, b) create_int(a->first.i * b->first.i)
#define mul_fop(a, b) create_float(a->first.f * b->first.f)
builtin_binop(mul, mul_iop, mul_fop)

#define div_iop(a, b) create_int(a->first.i / b->first.i)
#define div_fop(a, b) create_float(a->first.f / b->first.f)
builtin_binop(div, div_iop, div_fop)

#define mod_iop(a, b) create_int(a->first.i % b->first.i)
#define mod_fop(a, b) create_float(fmod(a->first.f, b->first.f))
builtin_binop(mod, mod_iop, mod_fop)

static void list_init(SnapList* list) {
  list->count = 0;
  list->prev = (SnapNode*)list;
  list->next = (SnapNode*)list;
}

static bool list_is_empty(SnapList* list) {
  return list->next == (SnapNode*)list;
}

static void list_remove(SnapList* list, SnapNode* node) {
  list->count--;
  node->prev->next = node->next;
  node->next->prev = node->prev;
  node->next = NULL;
  node->prev = NULL;
}

static void list_insert_after(SnapList* list, SnapNode* pos, SnapNode* node) {
  list->count++;
  pos->next->prev = node;
  node->prev = pos;
  node->next = pos->next;
  pos->next = node;
}

static void list_insert_before(SnapList* list, SnapNode* pos, SnapNode* node) {
  list->count++;
  pos->prev->next = node;
  node->next = pos;
  node->prev = pos->prev;
  pos->prev = node;
}

static void list_append(SnapList* list, SnapNode* node) {
  list_insert_after(list, (SnapNode*)list, node);
}

static void list_prepend(SnapList* list, SnapNode* node) {
  list_insert_before(list, (SnapNode*)list, node);
}

static SnapCodeGen* code_gen_new() {
  SnapCodeGen* code_gen = (SnapCodeGen*)malloc(sizeof(SnapCodeGen));
  list_init(&code_gen->insts);
  code_gen->scope = NULL;
  snap_hash_init(&code_gen->constants);
  snap_hash_init(&code_gen->global_names);
  code_gen->num_locals = 0;
  code_gen->max_stack_size = 0;
  code_gen->is_tail = false;
  code_gen->up = NULL;
  return code_gen;
}

static void code_gen_destroy(SnapCodeGen* code_gen) {
  SnapList* insts = &code_gen->insts;
  while (!list_is_empty(insts)) {
    SnapNode* temp = insts->next;
    list_remove(insts, temp);
    free(temp);
  }
  snap_hash_destroy(&code_gen->constants);
  snap_hash_destroy(&code_gen->global_names);
  free(code_gen);
}

static SnapInst* inst_new(int opcode, int arg) {
  SnapInst* inst = (SnapInst*)malloc(sizeof(SnapInst));
  inst->opcode = opcode;
  inst->arg = arg;
  inst->next = inst->prev = NULL;
  return inst;
}

static void insts_append(SnapList* insts, int opcode, int arg) {
  list_append(insts, (SnapNode*)inst_new(opcode, arg));
}

void snap_init(Snap* snap) {
  snap->anchored = (SObject**)malloc(32 * sizeof(SObject*));
  snap->anchored_capacity = 32;
  snap->anchored_top = 0;
  snap->num_bytes_alloced = 0;
  snap->num_bytes_alloced_last_gc = 0;
  snap->all = NULL;
  snap->gray = NULL;
  snap->trying = NULL;
  snap->cause = NULL;
  snap->frame = &snap->bottom_frame;
  snap->frame->scope = NULL;
  snap->frame->up = NULL;
  snap_hash_init(&snap->globals);

  snap_def_cfunc(snap, "apply", builtin_apply);
  snap_def_cfunc(snap, "nil?", builtin_isnil);
  snap_def_cfunc(snap, "empty?", builtin_isempty);
  snap_def_cfunc(snap, "cons", builtin_cons);
  snap_def_cfunc(snap, "first", builtin_first);
  snap_def_cfunc(snap, "car", builtin_first);
  snap_def_cfunc(snap, "rest", builtin_rest);
  snap_def_cfunc(snap, "cdr", builtin_rest);

  snap_def_cfunc(snap, "make-hash", builtin_make_hash);
  snap_def_cfunc(snap, "get", builtin_get);
  snap_def_cfunc(snap, "put!", builtin_put);
  snap_def_cfunc(snap, "del!", builtin_del);

  snap_def_cfunc(snap, "print", builtin_print);
  snap_def_cfunc(snap, "gc", builtin_gc);
  snap_def_cfunc(snap, "<", builtin_lt);
  snap_def_cfunc(snap, ">", builtin_gt);
  snap_def_cfunc(snap, "<=", builtin_le);
  snap_def_cfunc(snap, ">=", builtin_ge);
  snap_def_cfunc(snap, "=", builtin_eq);
  snap_def_cfunc(snap, "add", builtin_add);
  snap_def_cfunc(snap, "sub", builtin_sub);
  snap_def_cfunc(snap, "mul", builtin_mul);
  snap_def_cfunc(snap, "div", builtin_div);
  snap_def_cfunc(snap, "mod", builtin_mod);
}

void snap_destroy(Snap* snap) {
  SObject* obj = snap->all;
  while (obj) {
    SObject* temp = obj;
    obj = obj->next;
    gc_free(snap, temp);
  }
  free((void*)snap->anchored);
  snap_hash_destroy(&snap->globals);
  assert(snap->num_bytes_alloced == 0);
}
