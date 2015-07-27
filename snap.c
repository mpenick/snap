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

#define GC_EVERY_NUM_BYTES 8

enum {
  WHITE,
  GRAY,
  BLACK
};

#define check(a, e) (assert(a), (e))
#define is_obj(v) ((v).type > STYPE_CFUNC)

#define is_nil(v) ((v).type == STYPE_NIL)
#define is_bool(v) ((v).type == STYPE_BOOL)
#define is_int(v) ((v).type == STYPE_INT)
#define is_float(v) ((v).type == STYPE_FLOAT)
#define is_form(v) ((v).type == STYPE_FORM)
#define is_cfunc(v) ((v).type == STYPE_CFUNC)
#define is_sym(v) ((v).type == STYPE_SYM)
#define is_str(v) ((v).type == STYPE_STR)
#define is_err(v) ((v).type == STYPE_ERR)
#define is_cons(v) ((v).type == STYPE_CONS)
#define is_hash(v) ((v).type == STYPE_HASH)
#define is_scope(v) ((v).type == STYPE_SCOPE)
#define is_fn(v) ((v).type == STYPE_FN)

#define as_sym(v) check(is_sym(v), (SSymStr*)(v).o)
#define as_str(v) check(is_str(v), (SSymStr*)(v).o)
#define as_err(v) check(is_err(v), (SErr*)(v).o)
#define as_cons(v) check(is_cons(v), (SCons*)(v).o)
#define as_hash(v) check(is_hash(v), (SHash*)(v).o)
#define as_fn(v) check(is_fn(v), (SFn*)(v).o)

static bool read(Snap* snap, SnapLex* lex, SValue* val);
static SValue* lookup(Snap* snap, const char* name);
static SValue exec(Snap* snap, SValue val);

static SValue create_nil() {
  SValue val;
  val.type = STYPE_NIL;
  val.o = NULL;
  return val;
}

static SValue create_empty() {
  SValue val;
  val.type = STYPE_CONS;
  val.o = NULL;
  return val;
}

static SValue create_bool(bool b) {
  SValue val;
  val.type = STYPE_BOOL;
  val.b = b;
  return val;
}

static SValue create_int(SnapInt i) {
  SValue val;
  val.type = STYPE_INT;
  val.i = i;
  return val;
}

static SValue create_float(SnapFloat f) {
  SValue val;
  val.type = STYPE_FLOAT;
  val.f = f;
  return val;
}

static SValue create_cfunc(SCFunc c) {
  SValue val;
  val.type = STYPE_CFUNC;
  val.c = c;
  return val;
}

static SValue create_obj(SObject* o) {
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
      printf("free fn %p\n", obj);
      snap->num_bytes_alloced -= sizeof(SFn);
      break;
    default:
      assert(0 && "Not a GC object");
      break;
  }
  // TODO: Remove
  obj->type = 0xFF;
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
  size_t i;
  for (i = 0; i < hash->capacity; ++i) {
    SEntry* entry = &hash->entries[i];
    if (entry->key) gc_mark_val(snap, entry->val);
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

void snap_def(Snap* snap, const char* name, SValue val) {
  snap_hash_put(snap->frame->scope ? &snap->frame->scope->vars : &snap->globals, name, val);
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
      if (is_obj(val)) snap_push(snap, val.o);
      res = exec(snap, val);
      if (is_obj(val)) snap_pop(snap);
    }
  } else {
    fprintf(stderr, "Error '%s' (code: %d)\n",
            snap->cause->msg->data,
            snap->cause->code);
    res = create_obj((SObject*)snap->cause);
  }
  return res;
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

SFn* snap_fn_new(Snap* snap, SCons* params, SCons* body) {
  SFn* fn = (SFn*)gc_new(snap, STYPE_FN, sizeof(SFn));
  fn->name = NULL;
  fn->scope = snap->frame->scope;
  fn->params = params;
  fn->body = body;
  printf("fn %p\n", fn);
  return fn;
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

static bool read_val(Snap* snap, SnapLex* lex, int token, SValue* val);

static SCons* read_cons(Snap* snap, SnapLex* lex) {
  int token = snap_lex_next_token(lex);
  SCons* first = NULL;
  SCons** cons = &first;
  while (token != ')' && token != TK_EOF) {
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
  if (token != ')') {
    snap_throw(snap, 1, "Expected ')' at line %d", lex->line);
  }
  if (first) snap_pop(snap);
  return first;
}

static bool read_val(Snap* snap, SnapLex* lex, int token, SValue* val) {
  switch(token) {
    case '(':
      val->type = STYPE_CONS;
      val->o = (SObject*)read_cons(snap, lex);
      break;
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

static SValue* lookup(Snap* snap, const char* name) {
  SValue* res;
  SScope* scope = snap->frame->scope;
  while (scope) {
    res = snap_hash_get(&scope->vars, name);
    if (res) return res;
    scope = scope->up;
  }
  return snap_hash_get(&snap->globals, name);
}

static SValue lookup_sym(Snap* snap, SSymStr* sym) {
  SValue* val = lookup(snap, sym->data);
  if (val) return *val;
  snap_throw(snap, 0, "Unable to find symbol %s", sym->data);
  return create_nil();
}

static bool is_recur(SCons* cons) {
  return is_cons(cons->first) &&
      as_cons(cons->first) &&
      is_form(as_cons(cons->first)->first) &&
      as_cons(cons->first)->first.i == TK_RECUR;
}

static int arity(SCons* cons) {
  int n = 0;
  for (; cons; cons = as_cons(cons->rest)) n++;
  return n;
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
  SCons* param;
  if (!args ||
      !is_cons(args->first) ||
      !is_cons(args->rest)) {
    snap_throw(snap, 0, "Invalid fn");
  }
  for (param = as_cons(args->first); param; param = as_cons(param->rest)) {
    if (param->first.type != STYPE_SYM) {
      snap_throw(snap, 0, "Fn parameter is not a symbol");
    }
  }
  return create_obj((SObject*)snap_fn_new(snap,
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
                  as_sym(as_cons(arg->first)->first)->data,
                  exec(snap, as_cons(as_cons(arg->first)->rest)->first));
  }
  res = exec_body(snap, as_cons(args->rest));
  snap->frame->scope = snap->frame->scope->up;
  return res;
}

static SValue form_throw(Snap* snap, SCons* args) {
  SErr* err;
  if (!args ||
      !is_int(args->first) ||
      !is_cons(args->rest) || !as_cons(args->rest) ||
      !is_str(as_cons(args->rest)->first)) {
    snap_throw(snap, 0, "Invalid throw");
  }
  err = (SErr*)gc_new(snap, STYPE_ERR, sizeof(SErr));
  err->code = args->first.i;
  err->msg = as_str(as_cons(args->rest)->first);
  err->inner = snap->cause;
  snap->cause = err;
  longjmp(snap->trying->buf, 1);
  return create_nil(); /* Never happens */
}

static SValue form_try(Snap* snap, SCons* args) {
  SValue res;
  SnapTry trying;
  SScope* prev_scope;
  size_t prev_anchored_top;
  if (!args ||
      !is_cons(args->rest) || !as_cons(args->rest) ||
      !is_cons(as_cons(args->rest)->first)) {
    snap_throw(snap, 0, "Invalid try");
  }
  prev_scope = snap->frame->scope;
  prev_anchored_top = snap->anchored_top;
  trying.up = snap->trying;
  snap->trying = &trying;
  if(!setjmp(trying.buf)) {
    res = exec(snap, args->first);
    snap->trying = trying.up;
  } else {
    SValue handler;
    SCons* cons;
    snap->frame->scope = prev_scope;
    snap->anchored_top = prev_anchored_top;
    snap->trying = trying.up;
    handler = exec(snap, as_cons(args->rest)->first);
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
  val = lookup(snap, as_sym(args->first)->data);
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

bool bind_params(Snap* snap, SCons* params, SCons* args,
                 SScope* enclosed_scope) {
  SCons* param;
  SScope* new_scope = (SScope*)snap_push(snap,
                                         (SObject*)snap_scope_new(snap));
  new_scope->up = enclosed_scope;
  if(arity(params) != arity(args)) return false;
  for (param = params; param; param = as_cons(param->rest)) {
    snap_hash_put(&new_scope->vars,
                  as_sym(param->first)->data,
                  exec(snap, args->first));
    args = as_cons(args->rest);
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
  if (!bind_params(snap, fn->params, args, fn->scope)) {
    snap_throw(snap, 0, "Invalid number of arguments");
  }
  for (;;) {
    snap->tail = NULL;
    res = exec_body(snap, fn->body);
    if (!snap->tail) break;
    if (!bind_params(snap, fn->params,
                     as_cons(as_cons(snap->tail->first)->rest),
                     fn->scope)) {
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
  SValue first = exec(snap, cons->first);
  switch (first.type) {
    case STYPE_FORM:
      return exec_form(snap, first.i, as_cons(cons->rest));
    case STYPE_CFUNC:
      return exec_cfunc(snap,first.c, as_cons(cons->rest));
    case STYPE_FN:
      return exec_fn(snap, as_fn(first), as_cons(cons->rest));
    default:
      snap_throw(snap, 0, "Expected special form, C func or fn");
      return create_nil();
  }
}

static SValue exec(Snap* snap, SValue val) {
  switch (val.type) {
    case STYPE_NIL:
    case STYPE_BOOL:
    case STYPE_INT:
    case STYPE_FLOAT:
    case STYPE_FORM:
    case STYPE_STR:
    case STYPE_ERR:
    case STYPE_HASH:
      return val;
    case STYPE_SYM:
      return lookup_sym(snap, as_sym(val));
    case STYPE_CONS:
      return as_cons(val) ? exec_cons(snap, as_cons(val)) : val;
    default:
      snap_throw(snap, 0, "Invalid type");
      return create_nil();
  }
}

static void print_cons(SCons* cons);

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
  if (!args || !is_cons(args->first)) {
    snap_throw(snap, 0, "Invalid first");
  }
  return as_cons(args->first)->first;
}

static SValue builtin_rest(Snap* snap, SCons* args) {
  if (!args || !is_cons(args->first)) {
    snap_throw(snap, 0, "Invalid rest");
  }
  return as_cons(args->first)->rest;
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

  snap_def_cfunc(snap, "print", builtin_print);
  snap_def_cfunc(snap, "cons", builtin_cons);
  snap_def_cfunc(snap, "first", builtin_first);
  snap_def_cfunc(snap, "car", builtin_first);
  snap_def_cfunc(snap, "rest", builtin_rest);
  snap_def_cfunc(snap, "cdr", builtin_rest);
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
  SObject* o;
  for (o = snap->all; o; o = o->next) {
    gc_free(snap, o);
  }
  free((void*)snap->anchored);
  snap_hash_destroy(&snap->globals);
  assert(snap->num_bytes_alloced == 0);
}
