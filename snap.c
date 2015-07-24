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

#define GC_EVERY_NUM_BYTES (1 * 1024 * 1024)

enum {
  WHITE,
  GRAY,
  BLACK
};

#define check(a, e) (assert(a), (e))
#define is_gc(v) ((v).type > STYPE_CFUNC)
#define as_sym(v) check((v).type == STYPE_SYM, (SSymStr*)(v).o)
#define as_str(v) check((v).type == STYPE_STR, (SSymStr*)(v).o)
#define as_err(v) check((v).type == STYPE_ERR, (SErr*)(v).o)
#define as_cons(v) check((v).type == STYPE_CONS, (SCons*)(v).o)
#define as_hash(v) check((v).type == STYPE_HASH, (SHash*)(v).o)
#define as_fn(v) check((v).type == STYPE_FN, (SFn*)(v).o)

static bool read(Snap* snap, SnapLex* lex, SValue* val);
static SValue* lookup(Snap* snap, const char* name);
static SValue exec(Snap* snap, SValue val);

__attribute__((__format__(__printf__, 3, 4)))
static SValue create_error(Snap* snap, int code, const char* format, ...) {
  SValue val;
  char buf[256];
  val.type = STYPE_ERR;
  va_list args;
  va_start(args, format);
  vsnprintf(buf, sizeof(buf), format, args);
  va_end(args);
  val.o = (SObject*)snap_err_new(snap, code, buf);
  return val;
}

static SValue create_nil() {
  SValue val;
  val.type = STYPE_NIL;
  return val;
}

static SValue create_bool(bool b) {
  SValue val;
  val.type = STYPE_BOOL;
  val.b = b;
  return val;
}

static SValue create_int(int i) {
  SValue val;
  val.type = STYPE_INT;
  val.i = i;
  return val;
}

static SValue create_float(int f) {
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

static SValue create_obj(uint8_t type, SObject* o) {
  SValue val;
  val.type = type;
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
  if (is_gc(val)) gc_mark(snap, val.o);
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
      break;
    case STYPE_CONS:
      gc_mark_val(snap, ((SCons*)obj)->first);
      gc_mark(snap, (SObject*)((SCons*)obj)->rest);
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
      gc_mark(snap, (SObject*)((SFn*)obj)->params);
      gc_mark(snap, (SObject*)((SFn*)obj)->body);
      break;
    default:
      assert(0 && "Not a valid gray object");
      break;
  }
}

static void gc_collect(Snap* snap) {
  SScope* scope = snap->scope;
  SObject** obj;

  while (scope) {
    gc_mark(snap, (SObject*)scope);
    scope = scope->up;
  }

  for (obj = snap->anchored; obj < snap->anchored_top; ++obj) {
    gc_mark(snap, *obj);
  }

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
  snap_hash_put(&snap->scope->vars, name, val);
}

void snap_def_cfunc(Snap* snap, const char* name, SCFunc cfunc) {
  snap_def(snap, name, create_cfunc(cfunc));
}

SValue snap_exec(Snap* snap, const char* expr) {
  SValue res;
  SValue val;
  SnapLex lex;
  lex.buf = expr;
  lex.buf_size = strlen(lex.buf);
  lex.p = lex.buf;
  lex.line = 0;
  while (read(snap, &lex, &val)) {
    if (is_gc(val)) snap_push(snap, val.o);
    res = exec(snap, val);
    if (is_gc(val)) snap_pop(snap);
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
  snap_push(snap, (SObject*)e);
  e->code = code;
  e->msg = snap_str_new(snap, msg);
  snap_pop(snap);
  return e;
}

SCons* snap_cons_new(Snap* snap) {
 SCons* c = (SCons*)gc_new(snap,  STYPE_CONS, sizeof(SCons));
 c->first.type = STYPE_NIL;
 c->rest = NULL;
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
  fn->params = params;
  fn->body = body;
  return fn;
}

SObject* snap_push(Snap* snap, SObject* obj) {
  uintptr_t used = snap->anchored_top - snap->anchored;
  if (used >= snap->anchored_capacity) {
    snap->anchored_capacity *= 2;
    snap->anchored = (SObject**)realloc(snap->anchored, snap->anchored_capacity);
    snap->anchored_top = snap->anchored + used;
  }
  *snap->anchored_top++ =  obj;
  return obj;
}

void snap_pop(Snap* snap) {
  assert(snap->anchored_top >= snap->anchored);
  snap->anchored_top--;
}

static bool read_val(Snap* snap, SnapLex* lex, int token, SValue* val);

static SCons* read_list(Snap* snap, SnapLex* lex) {
  int token = snap_lex_next_token(lex);
  SCons* first = NULL;
  SCons** cons = &first;
  while (token != ')' && token != TK_EOF) {
    SValue val;
    *cons = (SCons*)snap_push(snap, (SObject*)snap_cons_new(snap));
    if (!read_val(snap, lex, token, &val)) {
      fprintf(stderr, "Premature EOF at %d\n", lex->line);
      exit(-1);
    }
    (*cons)->first = val;
    cons = &(*cons)->rest;
    token = snap_lex_next_token(lex);
    snap_pop(snap);
  }
  if (token != ')') {
    fprintf(stderr, "Parser error at %d\n", lex->line);
    exit(-1);
  }
  return first;
}

static bool read_val(Snap* snap, SnapLex* lex, int token, SValue* val) {
  switch(token) {
    case '(':
      val->type = STYPE_CONS;
      val->o = (SObject*)read_list(snap, lex);
      break;
    case TK_INT:
      *val = create_int(atoi(lex->val));
      break;
    case TK_FLOAT:
      *val = create_float(atof(lex->val));
      break;
    case TK_STR:
      *val = create_obj(STYPE_STR, (SObject*)snap_str_new(snap, lex->val));
      break;
    case TK_ID:
      *val = create_obj(STYPE_SYM, (SObject*)snap_sym_new(snap, lex->val));
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
      val->type = STYPE_FORM;
      val->i = token;
      break;
    case TK_EOF:
      return false;
    default:
      fprintf(stderr, "Parser error at %d\n", lex->line);
      exit(-1);
      break;
  }

  return true;
}

static bool read(Snap* snap, SnapLex* lex, SValue* val) {
  return read_val(snap, lex, snap_lex_next_token(lex), val);
}

static SValue* lookup(Snap* snap, const char* name) {
  SScope* scope = snap->scope;
  while (scope) {
    SValue* val = snap_hash_get(&scope->vars, name);
    if (val) return val;
    scope = scope->up;
  }
  return NULL;
}

static SValue lookup_sym(Snap* snap, SSymStr* sym) {
  SValue* val = lookup(snap, sym->data);
  if (val) return *val;
  return create_error(snap, 0, "Unable to find symbol %s", sym->data);
}

static bool is_recur(SCons* cons) {
  return cons->first.type == STYPE_CONS &&
      as_cons(cons->first)->first.type == STYPE_FORM &&
      as_cons(cons->first)->first.i == TK_RECUR;
}

static SValue exec_body(Snap* snap, SCons*body) {
  SValue res;
  for (; body; body = body->rest) {
    if (is_recur(body)) {
      if (body->rest) {
        return create_error(snap, 0, "Recur not in tail position");
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
  if (!args || args->first.type != STYPE_SYM || !args->rest) {
    return create_error(snap, 0, "Invalid def");
  }
  res = exec(snap, args->rest->first);
  if (res.type == STYPE_FN) {
    as_fn(res)->name = as_sym(args->first);
  }
  snap_def(snap,  as_sym(args->first)->data, res);
  return create_nil();
}

static SValue form_if(Snap* snap, SCons* args) {
  SValue res;
  SCons* body = args->rest;
  SValue cond;
  if (!args || !args->rest || !args->rest->rest) {
    return create_error(snap, 0, "Invalid if");
  }
  cond = exec(snap, args->first);
  if (cond.type == STYPE_NIL || (cond.type == STYPE_BOOL && !cond.b)) {
    body = body->rest;
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
  if (!args || !args->rest || args->first.type != STYPE_CONS) {
    return create_error(snap, 0, "Invalid fn");
  }
  for (param = as_cons(args->first); param; param = param->rest) {
    if (param->first.type != STYPE_SYM) {
      return create_error(snap, 0, "Fn parameter is not a symbol");
    }
  }
  return create_obj(STYPE_FN,
                       (SObject*)snap_fn_new(snap,
                                             as_cons(args->first),
                                             args->rest));
}

static SValue form_let(Snap* snap, SCons* args) {
  SValue res;
  SCons* arg;
  SScope* scope = snap_scope_new(snap);
  scope->up = snap->scope;
  snap->scope = scope;
  if (!args || args->first.type != STYPE_CONS || !args->rest) {
    return create_error(snap, 0, "Invalid let");
  }
  for (arg = as_cons(args->first); arg; arg = arg->rest) {
    if (arg->first.type != STYPE_CONS ||
        as_cons(arg->first)->first.type != STYPE_SYM ||
        !as_cons(arg->first)->rest) {
      return create_error(snap, 0, "Invalid let binding");
    }
    snap_hash_put(&scope->vars,
                  as_sym(as_cons(arg->first)->first)->data,
                  exec(snap, as_cons(arg->first)->rest->first));
  }
  res = exec_body(snap, args->rest);
  snap->scope = snap->scope->up;
  return res;
}

static SValue exec_set(Snap* snap, SCons* args) {
  SValue* val;
  if (!args || args->first.type != STYPE_SYM || !args->rest) {
    return create_error(snap, 0, "Invalid set!");
  }
  val = lookup(snap, as_sym(args->first)->data);
  if (!val) {
    return create_error(snap, 0, "Variable '%s' is not defined",
                        as_sym(args->first)->data);
  }
  *val = exec(snap, args->rest->first);
  return create_nil();
}

static SValue exec_cons(Snap* snap, SCons* cons);

static SValue exec_cfunc(Snap* snap, SCFunc cfunc, SCons* args) {
  SCons* arg = args;
  SCons* first = NULL;
  SCons** cons = &first;
  for (; arg; arg = arg->rest) {
    *cons = (SCons*)snap_push(snap, (SObject*)snap_cons_new(snap));
    (*cons)->first = exec(snap, arg->first);
    cons = &(*cons)->rest;
    snap_pop(snap);
  }
  return cfunc(snap, first);
}

static int calc_arity(SCons* cons) {
  int arity = 0;
  for (; cons; cons = cons->rest) arity++;
  return arity;
}

static SValue exec_fn(Snap* snap, SFn* fn, SCons* args) {
  SValue res;
  SCons* arg = args;
  SScope* scope = snap_scope_new(snap);
  scope->up = snap->scope;
  snap->scope = scope;
  for (;;) {
    SCons* param = fn->params;
    snap->tail = NULL;
    if (calc_arity(param) != calc_arity(arg)) {
      return create_error(snap, 0, "Invalid number of arguments");
    }
    for (; param; param = param->rest) {
      snap_hash_put(&scope->vars, as_sym(param->first)->data, exec(snap, arg->first));
      arg = arg->rest;
    }
    res = exec_body(snap, fn->body);
    if (!snap->tail) break;
    arg = as_cons(snap->tail->first)->rest;
  }
  snap->scope = snap->scope->up;
  return res;
}

static SValue exec_form(Snap* snap, int form, SCons *args) {
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
      return args->rest->first;
    case TK_SET:
      return exec_set(snap, args);
    default:
      return create_error(snap, 0, "Invalid primitive form");
  }
}

static SValue exec_cons(Snap* snap, SCons* cons) {
  SValue first = exec(snap, cons->first);
  switch (first.type) {
    case STYPE_FORM:
      return exec_form(snap, first.i, cons->rest);
    case STYPE_CFUNC:
      return exec_cfunc(snap,first.c, cons->rest);
    case STYPE_FN:
      return exec_fn(snap, as_fn(first), cons->rest);
    default:
      return create_error(snap, 0, "Expected special form, C func or fn");
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
      return exec_cons(snap, as_cons(val));
    default:
      return create_error(snap, 0, "Invalid type");
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
      printf("%d", val.i);
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
      printf("\n(");
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
    cons = cons->rest;
  }
}

static SValue builtin_print(Snap* snap, SCons* args) {
  SCons* arg;
  for (arg = args; arg; arg = arg->rest) {
    snap_print(arg->first);
  }
  printf("\n");
  return create_nil();
}

#define builtin_binop(name, iop, fop) \
static SValue builtin_##name(Snap* snap, SCons* args) { \
  SCons* a = args; \
  SCons* b; \
  if (!args || !args->rest) { \
    return create_error(snap, 0, \
      "Binary function requires two parameters"); \
  } \
  b = args->rest; \
  if (a->first.type == STYPE_INT && \
      b->first.type == STYPE_INT) { \
    return iop(a, b); \
  } else if (a->first.type == STYPE_FLOAT && \
             b->first.type == STYPE_FLOAT) { \
    return fop(a, b); \
  } else { \
    return create_error(snap, 0, "Incompatible types"); \
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
#define ge_fop(a, b) create_bool(a->first.f >- b->first.f)
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
  snap->anchored_top = snap->anchored;
  snap->num_bytes_alloced_last_gc = 0;
  snap->all = NULL;
  snap->gray = NULL;
  snap->scope = snap_scope_new(snap);

  snap_def_cfunc(snap, "print", builtin_print);
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
  free((void*)snap->anchored);
  for (o = snap->all; o; o = o->next) {
    gc_free(snap, o);
  }
}
