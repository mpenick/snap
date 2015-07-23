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

SValue read(Snap* snap, SnapLex* lex);
SValue exec(Snap* snap, SValue val);

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

static SValue create_object(uint8_t type, SObject* o) {
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
      gc_mark(snap, (SObject*)((SFn*)obj)->params);
      gc_mark(snap, (SObject*)((SFn*)obj)->body);
      gc_mark(snap, (SObject*)((SFn*)obj)->scope);
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

void snap_def_func(Snap* snap, const char* name, SCFunc func) {
  SValue val;
  val.type = STYPE_CFUNC;
  val.c = func;
  snap_def(snap, name, val);
}

SValue snap_exec(Snap* snap, const char* expr) {
  SnapLex lex;
  lex.buf = expr;
  lex.buf_size = strlen(lex.buf);
  lex.p = lex.buf;
  lex.line = 0;
  return exec(snap, read(snap, &lex));
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
  SFn* l = (SFn*)gc_new(snap, STYPE_FN, sizeof(SFn));
  l->params = params;
  l->body = body;
  l->scope = NULL;
  return l;
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

SValue read_val(Snap* snap, SnapLex* lex, int token);

SCons* read_list(Snap* snap, SnapLex* lex) {
  int token = snap_lex_next_token(lex);
  SCons* first = NULL;
  SCons** cons = &first;
  while (token != ')' && token != TK_EOF) {
    *cons = (SCons*)snap_push(snap, (SObject*)snap_cons_new(snap));
    (*cons)->first = read_val(snap, lex, token);
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

SValue read_val(Snap* snap, SnapLex* lex, int token) {
  SValue v;

  switch(token) {
    case '(':
      v.type = STYPE_CONS;
      v.o = (SObject*)read_list(snap, lex);
      break;
    case TK_INT:
      v.type = STYPE_INT;
      v.i = atoi(lex->val);
      break;
    case TK_FLOAT:
      v.type = STYPE_FLOAT;
      v.f = atof(lex->val);
      break;
    case TK_STR:
      v.type = STYPE_STR;
      v.o = (SObject*)snap_str_new(snap, lex->val);
      break;
    case TK_ID:
      v.type = STYPE_SYM;
      v.o = (SObject*)snap_sym_new(snap, lex->val);
      break;
    default:
      fprintf(stderr, "Parser error at %d\n", lex->line);
      exit(-1);
      break;
  }

  return v;
}

SValue read(Snap* snap, SnapLex* lex) {
  return read_val(snap, lex, snap_lex_next_token(lex));
}

bool lookup(Snap* snap, const char* sym, SValue* val) {
  SScope* scope = snap->scope;
  while (scope) {
    if (snap_hash_get(&scope->vars, sym, val)) {
      return true;
    }
    scope = scope->up;
  }
  return false;
}

SValue def(Snap* snap, SCons* args) {
  SCons* a = args;
  SCons* b;
  if (!args || !args->rest) {
    return create_error(snap, SERR_PARSE, "Invalid def");
  }
  b = args->rest;
  if (a->type != STYPE_SYM) {
    return create_error(snap, SERR_PARSE, "First parameter must be a symbol");
  }
  snap_def(snap,  as_str(a->first)->data, b->first);
  return create_nil();
}

SValue fn(Snap* snap, SCons* args) {
  SCons* param;
  if (!args || !args->rest || args->first.type != STYPE_CONS) {
    return create_error(snap, SERR_PARSE, "Invalid fn");
  }
  for (param = as_cons(args->first); param; param = param->rest) {
    if (param->first.type != STYPE_SYM) {
      return create_error(snap, SERR_PARSE, "Fn parameter is not a symbol");
    }
  }
  return create_object(STYPE_FN,
                       (SObject*)snap_fn_new(snap,
                                             as_cons(args->first),
                                             args->rest));
}

SValue let(Snap* snap, SCons* args) {
  SScope* scope = snap_scope_new(snap);
  scope->up = snap->scope;
  snap->scope = scope;

  snap->scope = snap->scope->up;
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

SValue exec_sym(Snap* snap, SSymStr* sym, SCons* args) {
  SValue exec;
  char* id = sym->data;
  if (strcmp(id, "def") == 0) {
    return def(snap, args);
  } else if (strcmp(id, "fn") == 0) {
    return fn(snap, args);
  } else if (strcmp(id, "let") == 0) {
    return let(snap, args);
  } else if (strcmp(id, "quote") == 0) {
    return args->rest->first;
  } else if (lookup(snap, sym->data, &exec)) {
    switch(exec.type) {
      case STYPE_CFUNC:
        return exec_cfunc(snap, exec.c, args);
      case STYPE_FN:
        return exec_fn(snap, as_fn(exec), args);
      default:
        return create_error(snap, SERR_PARSE, "Expected a C function or fn");
    }
  }
  return create_error(snap, SERR_PARSE, "Unable to find symbol %s",
                      sym->data);
}

SValue exec_cons(Snap* snap, SCons* cons) {
  SValue first = exec(snap, cons->first);
  switch (first.type) {
    case STYPE_SYM:
      return exec_sym(snap, as_sym(first), cons->rest);
    case STYPE_FN:
      return exec_fn(snap, as_fn(first), cons->rest);
    default:
      return create_error(snap, SERR_PARSE, "Expected symbol or fn");
      break;
  }
}

SValue exec(Snap* snap, SValue val) {
  switch (val.type) {
    case STYPE_NIL:
    case STYPE_INT:
    case STYPE_FLOAT:
    case STYPE_SYM:
    case STYPE_STR:
    case STYPE_ERR:
    case STYPE_HASH:
      return val;
    case STYPE_CONS:
        return exec_cons(snap, as_cons(val));
    default:
      return create_error(snap, SERR_PARSE, "Invalid type");
  }
}

void print_list(SCons* cons);

void print(SValue val) {
  switch(val.type) {
    case STYPE_INT:
      printf("%d", val.i);
      break;
    case STYPE_FLOAT:
      printf("%f", val.f);
      break;
    case STYPE_STR:
      printf("%s", as_str(val)->data);
      break;
    case STYPE_SYM:
      printf("%s", as_sym(val)->data);
      break;
    case STYPE_CONS:
      printf("\n(");
      print_list(as_cons(val));
      printf(")");
      break;
  }
}

void print_list(SCons* cons) {
  int space = 0;
  while (cons != NULL) {
    if (space) printf(" ");
    space = 1;
    print(cons->first);
    cons = cons->rest;
  }
}

SValue add_(Snap* snap, SCons* args) {
  SCons* a = args;
  SCons* b;
  if (!args || !args->rest) {
    return create_error(snap, SERR_PARSE, "Binary function requires two parameters");
  }
  b = args->rest;
  if (a->first.type == STYPE_INT && b->first.type == STYPE_INT) {
    return create_int(a->first.i + b->first.i);
  } else if (a->first.type == STYPE_FLOAT && b->first.type == STYPE_FLOAT) {
    return create_float(a->first.f + b->first.f);
  } else {
    return create_error(snap, SERR_PARSE, "Incompatible types");
  }
}

SValue sub_(Snap* snap, SCons* args) {
  SCons* a = args;
  SCons* b;
  if (!args || !args->rest) {
    return create_error(snap, SERR_PARSE, "Binary function requires two parameters");
  }
  b = args->rest;
  if (a->first.type == STYPE_INT && b->first.type == STYPE_INT) {
    return create_int(a->first.i - b->first.i);
  } else if (a->first.type == STYPE_FLOAT && b->first.type == STYPE_FLOAT) {
    return create_float(a->first.f - b->first.f);
  } else {
    return create_error(snap, SERR_PARSE, "Incompatible types");
  }
}

SValue mul_(Snap* snap, SCons* args) {
  SCons* a = args;
  SCons* b;
  if (!args || !args->rest) {
    return create_error(snap, SERR_PARSE, "Binary function requires two parameters");
  }
  b = args->rest;
  if (a->first.type == STYPE_INT && b->first.type == STYPE_INT) {
    return create_int(a->first.i * b->first.i);
  } else if (a->first.type == STYPE_FLOAT && b->first.type == STYPE_FLOAT) {
    return create_float(a->first.f * b->first.f);
  } else {
    return create_error(snap, SERR_PARSE, "Incompatible types");
  }
}

SValue div_(Snap* snap, SCons* args) {
  SCons* a = args;
  SCons* b;
  if (!args || !args->rest) {
    return create_error(snap, SERR_PARSE, "Binary function requires two parameters");
  }
  b = args->rest;
  if (a->first.type == STYPE_INT && b->first.type == STYPE_INT) {
    return create_int(a->first.i / b->first.i);
  } else if (a->first.type == STYPE_FLOAT && b->first.type == STYPE_FLOAT) {
    return create_float(a->first.f / b->first.f);
  } else {
    return create_error(snap, SERR_PARSE, "Incompatible types");
  }
}

SValue mod_(Snap* snap, SCons* args) {
  SCons* a = args;
  SCons* b;
  if (!args || !args->rest) {
    return create_error(snap, SERR_PARSE, "Binary function requires two parameters");
  }
  b = args->rest;
  if (a->first.type == STYPE_INT && b->first.type == STYPE_INT) {
    return create_int(a->first.i % b->first.i);
  } else if (a->first.type == STYPE_FLOAT && b->first.type == STYPE_FLOAT) {
    return create_float(fmod(a->first.f,  b->first.f));
  } else {
    return create_error(snap, SERR_PARSE, "Incompatible types");
  }
}

int main(int argc, char** argv) {
  {
    Snap snap;

    snap_init(&snap);
    snap_def_func(&snap, "add", add_);
    snap_def_func(&snap, "sub", sub_);
    snap_def_func(&snap, "mul", mul_);
    snap_def_func(&snap, "div", div_);
    snap_def_func(&snap, "mod", mod_);

    print(snap_exec(&snap, "((fn () (add 1 2)))"));
    puts("\n");

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
