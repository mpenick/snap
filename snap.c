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

#define GC_EVERY_NUM_BYTES (8 * 1024 * 1024)

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

bool read(Snap* snap, SnapLex* lex, SValue* val);
SValue exec(Snap* snap, SValue val);
SValue* lookup(Snap* snap, const char* name);

__attribute__((__format__(__printf__, 3, 4)))
static inline SValue create_error(Snap* snap, int code, const char* format, ...) {
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

static inline SValue create_nil() {
  SValue val;
  val.type = STYPE_NIL;
  return val;
}

static inline SValue create_bool(bool b) {
  SValue val;
  val.type = STYPE_BOOL;
  val.b = b;
  return val;
}

static inline SValue create_int(int i) {
  SValue val;
  val.type = STYPE_INT;
  val.i = i;
  return val;
}

static inline SValue create_float(int f) {
  SValue val;
  val.type = STYPE_FLOAT;
  val.f = f;
  return val;
}

static inline SValue create_cfunc(SCFunc c) {
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

static inline  void gc_mark_val(Snap* snap, SValue val) {
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
    SObject* g = snap->gray;
    snap->gray = snap->gray->next;
    gc_mark_children(snap, g);
  }

  obj = &snap->all;

  while (obj) {
    if ((*obj)->mark == WHITE) {
      gc_free(snap, *obj);
      *obj = (*obj)->next;
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
  obj->next = snap->all;
  snap->all = obj;
  snap->num_bytes_alloced += size;
  snap->num_bytes_alloced_last_gc += size;
  return obj;
}

void snap_init(Snap* snap) {
  snap->anchored = (SObject**)malloc(32 * sizeof(SObject*));
  snap->anchored_capacity = 32;
  snap->anchored_top = snap->anchored;
  snap->num_bytes_alloced_last_gc = 0;
  snap->all = NULL;
  snap->gray = NULL;
  snap->scope = snap_scope_new(snap);
}

void snap_destroy(Snap* snap) {
  SObject* o;
  free((void*)snap->anchored);
  for (o = snap->all; o; o = o->next) {
    gc_free(snap, o);
  }
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
    res = exec(snap, val);
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

bool read_val(Snap* snap, SnapLex* lex, int token, SValue* val);

SCons* read_list(Snap* snap, SnapLex* lex) {
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
    snap_pop(snap);
    cons = &(*cons)->rest;
    token = snap_lex_next_token(lex);
  }
  if (token != ')') {
    fprintf(stderr, "Parser error at %d\n", lex->line);
    exit(-1);
  }
  return first;
}

bool read_val(Snap* snap, SnapLex* lex, int token, SValue* val) {
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

bool read(Snap* snap, SnapLex* lex, SValue* val) {
  return read_val(snap, lex, snap_lex_next_token(lex), val);
}

SValue do_(Snap* snap, SCons* args) {
  SValue res;
  SCons* cons;
  for (cons = args; cons; cons = cons->rest) {
    res = exec(snap, cons->first);
  }
  return res;
}

SValue def_(Snap* snap, SCons* args) {
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

SValue if_(Snap* snap, SCons* args) {
  if (!args || !args->rest || !args->rest->rest) {
    return create_error(snap, 0, "Invalid if");
  }
  if (args->first.type == STYPE_NIL ||
      (args->first.type == STYPE_BOOL && !args->first.b)) {
    return exec(snap, args->rest->rest->first);
  } else {
    return exec(snap, args->rest->first);
  }
}

SValue fn_(Snap* snap, SCons* args) {
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

SValue let_(Snap* snap, SCons* args) {
  SValue res;
  SCons* arg;
  SCons* cons;
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
  for (cons = args->rest; cons; cons = cons->rest) {
    res = exec(snap, cons->first);
  }
  snap->scope = snap->scope->up;
  return res;
}

SValue set_(Snap* snap, SCons* args) {
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

SValue exec_cons(Snap* snap, SCons* cons);

SValue exec_cfunc(Snap* snap, SCFunc cfunc, SCons* args) {
  SCons* arg = args;
  SCons* first = NULL;
  SCons** cons = &first;
  for (; arg; arg = arg->rest) {
    *cons = snap_cons_new(snap);
    (*cons)->first = exec(snap, arg->first);
    cons = &(*cons)->rest;
  }
  return cfunc(snap, first);
}

SValue exec_fn(Snap* snap, SFn* fn, SCons* args) {
  SValue res;
  SCons* param = fn->params;
  SCons* arg = args;
  SCons* cons = fn->body;
  SScope* scope = snap_scope_new(snap);
  scope->up = snap->scope;
  snap->scope = scope;
  for (; param; param = param->rest) {
    if (arg) {
      snap_hash_put(&scope->vars, as_str(param->first)->data, exec(snap, arg->first));
    } else {
      snap_hash_put(&scope->vars, as_str(param->first)->data, create_nil());
    }
  }
  for (; cons; cons = cons->rest) {
    res = exec(snap, cons->first);
  }
  snap->scope = snap->scope->up;
  return res;
}

SValue exec_form(Snap* snap, int form, SCons *args) {
  switch (form) {
    case TK_DO:
      return do_(snap, args);
    case TK_DEF:
      return def_(snap, args);
    case TK_IF:
      return if_(snap, args);
    case TK_FN:
      return fn_(snap, args);
    case TK_LET:
      return let_(snap, args);
    case TK_QUOTE:
      return args->rest->first;
    case TK_SET:
      return set_(snap, args);
    default:
      return create_error(snap, 0, "Invalid primitive form");
  }
}

SValue exec_cons(Snap* snap, SCons* cons) {
  SValue first = exec(snap, cons->first);
  switch (first.type) {
    case STYPE_FORM:
      return exec_form(snap, first.i, cons->rest);
    case STYPE_CFUNC:
      return exec_cfunc(snap,first.c, cons->rest);
    case STYPE_FN:
      return exec_fn(snap, as_fn(first), cons->rest);
    case STYPE_ERR:
      return first;
    default:
      return create_error(snap, 0, "Expected special form, C func or fn");
  }
}

SValue* lookup(Snap* snap, const char* name) {
  SScope* scope = snap->scope;
  while (scope) {
    SValue* val = snap_hash_get(&scope->vars, name);
    if (val) return val;
    scope = scope->up;
  }
  return NULL;
}

SValue lookup_sym(Snap* snap, SSymStr* sym) {
  SValue* val = lookup(snap, sym->data);
  if (val) return *val;
  return create_error(snap, 0, "Unable to find symbol %s", sym->data);
}

SValue exec(Snap* snap, SValue val) {
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

void print_cons(SCons* cons);

void print_val(SValue val) {
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
      printf("%s", as_str(val)->data);
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

void print_cons(SCons* cons) {
  int space = 0;
  while (cons != NULL) {
    if (space) printf(" ");
    space = 1;
    print_val(cons->first);
    cons = cons->rest;
  }
}

SValue print_(Snap* snap, SCons* args) {
  SCons* arg;
  for (arg = args; arg; arg = arg->rest) {
    print_val(arg->first);
  }
  printf("\n");
  return create_nil();
}

SValue add_(Snap* snap, SCons* args) {
  SCons* a = args;
  SCons* b;
  if (!args || !args->rest) {
    return create_error(snap, 0, "Binary function requires two parameters");
  }
  b = args->rest;
  if (a->first.type == STYPE_INT && b->first.type == STYPE_INT) {
    return create_int(a->first.i + b->first.i);
  } else if (a->first.type == STYPE_FLOAT && b->first.type == STYPE_FLOAT) {
    return create_float(a->first.f + b->first.f);
  } else {
    return create_error(snap, 0, "Incompatible types");
  }
}

SValue sub_(Snap* snap, SCons* args) {
  SCons* a = args;
  SCons* b;
  if (!args || !args->rest) {
    return create_error(snap, 0, "Binary function requires two parameters");
  }
  b = args->rest;
  if (a->first.type == STYPE_INT && b->first.type == STYPE_INT) {
    return create_int(a->first.i - b->first.i);
  } else if (a->first.type == STYPE_FLOAT && b->first.type == STYPE_FLOAT) {
    return create_float(a->first.f - b->first.f);
  } else {
    return create_error(snap, 0, "Incompatible types");
  }
}

SValue mul_(Snap* snap, SCons* args) {
  SCons* a = args;
  SCons* b;
  if (!args || !args->rest) {
    return create_error(snap, 0, "Binary function requires two parameters");
  }
  b = args->rest;
  if (a->first.type == STYPE_INT && b->first.type == STYPE_INT) {
    return create_int(a->first.i * b->first.i);
  } else if (a->first.type == STYPE_FLOAT && b->first.type == STYPE_FLOAT) {
    return create_float(a->first.f * b->first.f);
  } else {
    return create_error(snap, 0, "Incompatible types");
  }
}

SValue div_(Snap* snap, SCons* args) {
  SCons* a = args;
  SCons* b;
  if (!args || !args->rest) {
    return create_error(snap, 0, "Binary function requires two parameters");
  }
  b = args->rest;
  if (a->first.type == STYPE_INT && b->first.type == STYPE_INT) {
    return create_int(a->first.i / b->first.i);
  } else if (a->first.type == STYPE_FLOAT && b->first.type == STYPE_FLOAT) {
    return create_float(a->first.f / b->first.f);
  } else {
    return create_error(snap, 0, "Incompatible types");
  }
}

SValue mod_(Snap* snap, SCons* args) {
  SCons* a = args;
  SCons* b;
  if (!args || !args->rest) {
    return create_error(snap, 0, "Binary function requires two parameters");
  }
  b = args->rest;
  if (a->first.type == STYPE_INT && b->first.type == STYPE_INT) {
    return create_int(a->first.i % b->first.i);
  } else if (a->first.type == STYPE_FLOAT && b->first.type == STYPE_FLOAT) {
    return create_float(fmod(a->first.f,  b->first.f));
  } else {
    return create_error(snap, 0, "Incompatible types");
  }
}

int main(int argc, char** argv) {
  {
    Snap snap;

    snap_init(&snap);
    snap_def_cfunc(&snap, "print", print_);
    snap_def_cfunc(&snap, "add", add_);
    snap_def_cfunc(&snap, "sub", sub_);
    snap_def_cfunc(&snap, "mul", mul_);
    snap_def_cfunc(&snap, "div", div_);
    snap_def_cfunc(&snap, "mod", mod_);

    //print_val(snap_exec(&snap, "((fn () (add 1 2)))"));
    print_val(snap_exec(&snap, "(def foo (fn () (add 1 2)))(print foo)(print (foo))"));
    //print_val(snap_exec(&snap, "(let ((a 1) (b 2)) (add a b))"));
    //print_val(snap_exec(&snap, "(def x 1)\n(set! x 2)(add x 1)"));
    //print_val(snap_exec(&snap, "(do (if nil (add 1 1) (add 2 3)) (sub 2 1) (print 1))"));
    printf("\n");

    snap_destroy(&snap);
  }

#if 0
  {
    SnapHash h;

    const char** key;
    const char* keys[] = { "abcdefghijkl", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", NULL };

    snap_hash_init(&h);

    int i = 0;
    for (key = keys; *key; ++key) {
      SValue v;
      v.i = i++;
      snap_hash_put(&h, *key, v);
    }

    for (key = keys; *key; ++key) {
      SValue v;
      snap_hash_get(&h, *key, &v);
      printf("%s %d\n", *key, v.i);
    }

    for (key = keys; *key; ++key) {
      SValue v;
      snap_hash_delete(&h, *key);
    }

    snap_hash_destroy(&h);
  }
#endif

  //int token;

  //while ((token = lex_next_token(&lex)) != TK_EOF) {
  //  if (token == TK_INT) {
  //    printf("int(%s) %d\n", lex.val, atoi(lex.val));
  //  } else if (token == TK_FLOAT) {
  //    printf("float(%s) %f\n", lex.val, atof(lex.val));
  //  } else if (token == TK_STR) {
  //    printf("str %s\n", lex.val);
  //  } else {
  //    printf("other(%d) %s\n", token, lex.val);
  //  }
  //}

  return 0;
}
