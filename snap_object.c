// Values and objects: hashing, constructors, maps, vectors, lists, structs
// and printing
#include "snap_object.h"
#include "snap_compile.h"

#include <limits.h>
#include <math.h>

#define compare(a, b) ((a) < (b) ? -1 : (a) > (b))

static int hash_float(double n) {
  int i;
  unsigned int u;
  if (!isfinite(n)) return 0;
  n = frexp(n, &i) * -(double)INT_MIN;
  u = (unsigned int)n + (unsigned int)i;
  return (int)(u <= (unsigned int)INT_MAX ? u : ~u);
}

static int hash_chars(const char* data, size_t len) {
  return (int)mhash_fnv1a((const void*)data, len);
}

static int compare_chars(const char* data1, size_t len1, const char* data2, size_t len2) {
  if (len1 != len2) {
    return len1 < len2 ? -1 : 1;
  }
  return strncmp(data1, data2, len1);
}

static int compare_tempstring(const STempStr* ts, const SValue* val) {
  switch (snap_type(*val)) {
    case STYPE_SYM:
      return compare_chars(ts->data, ts->len, as_sym(*val)->data, as_sym(*val)->len);
    case STYPE_STR:
      return compare_chars(ts->data, ts->len, as_str(*val)->data, as_str(*val)->len);
    case STYPE_KEY:
      return compare_chars(ts->data, ts->len, as_key(*val)->data, as_key(*val)->len);
    default:
      return STYPE_TEMPSTR < snap_type(*val) ? -1 : 1;
  }
}

static int hash_pointer(void* ptr) {
  uintptr_t p = (uintptr_t)ptr;
  return (int)((p >> 4) | (p << (8 * sizeof(ptr) - 4)));
}

int snap_hash(const SValue* val) {
  SObject* obj = as_obj(*val);
  if (is_float(*val)) {
    return hash_float(as_float(*val)); // By value, so that 0.0 and -0.0 match
  }
  if (!is_obj(*val)) {
    return (int)(val->bits ^ (val->bits >> 32)); // Ints, nil, bools, forms and cfuncs
  }
  switch (obj->type) {
    // Hashed by contents so interning can look them up with a STempStr
    case STYPE_SYM:
      return hash_chars(((SSym*)obj)->data, ((SSym*)obj)->len);
    case STYPE_STR:
      return hash_chars(((SStr*)obj)->data, ((SStr*)obj)->len);
    case STYPE_KEY:
      return hash_chars(((SKeyword*)obj)->data, ((SKeyword*)obj)->len);
    case STYPE_TEMPSTR:
      return hash_chars(((STempStr*)obj)->data, ((STempStr*)obj)->len);
    // The rest match snap_compare(): errors by kind, lists, vectors, maps and
    // structs by contents
    case STYPE_ERR:
      return snap_hash(&((SErr*)obj)->err);
    case STYPE_CONS: {
      SCons* l;
      unsigned h = 1;
      for (l = (SCons*)obj; l; l = as_cons(l->rest)) h = h * 31 + snap_hash(&l->first);
      return (int)h;
    }
    case STYPE_VEC: {
      SVec* v = (SVec*)obj;
      unsigned h = 1;
      int i;
      for (i = 0; i < v->len; ++i) h = h * 31 + snap_hash(snap_vec_at(v, i));
      return (int)h;
    }
    case STYPE_MAP: {
      SMap* m = (SMap*)obj;
      unsigned h = 0;
      int i;
      for (i = 0; i < m->used; ++i) {
        SValue* e = &m->entries[2 * i];
        // Added, so the order doesn't matter
        if (e->bits != SV_DELETED) h += (unsigned)snap_hash(e) * 31 ^ snap_hash(e + 1);
      }
      return (int)h;
    }
    case STYPE_STRUCT: {
      SStruct* st = (SStruct*)obj;
      unsigned h = hash_pointer(st->stype);
      int i;
      for (i = 0; i < st->stype->num_fields; ++i) h = h * 31 + snap_hash(&st->vals[i]);
      return (int)h;
    }
    default:
      return hash_pointer(obj);
  }
}

int snap_compare(const SValue* val1, const SValue* val2) {
  int type1 = snap_type(*val1), type2 = snap_type(*val2);
  assert(type2 != STYPE_TEMPSTR);

  if (type1 == STYPE_TEMPSTR) {
    return compare_tempstring((STempStr*)as_obj(*val1), val2);
  } else if (type1 != type2) {
    return type1 < type2 ? -1 : 1;
  }

  switch (type1) {
    case STYPE_INT:
      return compare((SnapInt)val1->bits, (SnapInt)val2->bits);
    case STYPE_FLOAT:
      return compare(as_float(*val1), as_float(*val2));
    case STYPE_STR:
      return compare_chars(as_str(*val1)->data, as_str(*val1)->len,
                           as_str(*val2)->data, as_str(*val2)->len);
    case STYPE_ERR:
      return snap_compare(&as_err(*val1)->err, &as_err(*val2)->err);
    case STYPE_CONS: {
      // By contents, element by element
      SCons* l1 = as_cons(*val1), * l2 = as_cons(*val2);
      int c;
      for (; l1 && l2; l1 = as_cons(l1->rest), l2 = as_cons(l2->rest)) {
        if ((c = snap_compare(&l1->first, &l2->first)) != 0) return c;
      }
      return compare(l1 != NULL, l2 != NULL);
    }
    case STYPE_VEC: {
      // By contents, element by element
      SVec* v1 = as_vec(*val1), * v2 = as_vec(*val2);
      int i, c;
      for (i = 0; i < v1->len && i < v2->len; ++i) {
        if ((c = snap_compare(snap_vec_at(v1, i), snap_vec_at(v2, i))) != 0) return c;
      }
      return compare(v1->len, v2->len);
    }
    case STYPE_MAP: {
      // Equal when they have the same entries, otherwise ordered arbitrarily
      SMap* m1 = as_map(*val1), * m2 = as_map(*val2);
      int i;
      if (m1->len != m2->len) return compare(m1->len, m2->len);
      for (i = 0; i < m1->used; ++i) {
        SValue* e = &m1->entries[2 * i];
        SValue* v2;
        if (e->bits == SV_DELETED) continue;
        v2 = map_get(m2, *e);
        if (!v2 || snap_compare(e + 1, v2) != 0) {
          return compare((uintptr_t)m1, (uintptr_t)m2);
        }
      }
      return 0;
    }
    case STYPE_STRUCT: {
      // By type, then field by field
      SStruct* s1 = as_struct(*val1), * s2 = as_struct(*val2);
      int i, c;
      if (s1->stype != s2->stype) return compare((uintptr_t)s1->stype, (uintptr_t)s2->stype);
      for (i = 0; i < s1->stype->num_fields; ++i) {
        if ((c = snap_compare(&s1->vals[i], &s2->vals[i])) != 0) return c;
      }
      return 0;
    }
    default:
      // Everything else, including interned symbols and keywords, by identity
      return compare(val1->bits, val2->bits);
  }
}

static inline MHASH_FUNC(SnapHashEntry, SValue) {
  return snap_hash(&key);
}

static inline MHASH_EQUALS_FUNC(SnapHashEntry, SValue) {
  return snap_compare(&key1, &key2) == 0;
}

void snap_hash_init(SnapHash* hash) {
  mhash_init(hash, SnapHashEntry, create_undef(), snap_val(SV_DELETED), 0);
}

void snap_hash_put(SnapHash* hash, SValue key, SValue val) {
  SnapHashEntry* entry;
  mhash_find(hash, SnapHashEntry, key, entry, 1);
  entry->val = val;
  if (mhash_is_empty(hash, SnapHashEntry, entry) ||
      mhash_is_deleted(hash, SnapHashEntry, entry)) {
    entry->hkey = key;
    mhash_add(hash, SnapHashEntry);
  }
}

SValue* snap_hash_get(SnapHash *hash, SValue key) {
  SnapHashEntry* entry;
  mhash_find(hash, SnapHashEntry, key, entry, 0);
  if (mhash_is_empty(hash, SnapHashEntry, entry)) {
    return NULL;
  }
  return &entry->val;
}

SValue* snap_hash_get_str(SnapHash* hash, const char* key, size_t len) {
  STempStr ts;
  ts.type = STYPE_TEMPSTR;
  ts.data = key;
  ts.len = len;
  return snap_hash_get(hash, create_obj(&ts));
}

void snap_hash_destroy(SnapHash* hash) {
  mhash_destroy(hash);
}

/* Objects */

SValue create_float(Snap* snap, SnapFloat f) {
  return float_new(snap, f);
}

static SStr* str_new(Snap* snap, const char* data, size_t len) {
  SStr* s = (SStr*)gc_new(snap, STYPE_STR, sizeof(SStr) + len + 1);
  s->len = len;
  memcpy(s->data, data, len);
  s->data[len] = '\0';
  return s;
}

SStr* snap_str_new(Snap* snap, const char* str) {
  return str_new(snap, str, strlen(str));
}

SStr* snap_str_new_vformat(Snap* snap, const char* format, va_list args) {
  char buf[256]; /* TODO: Dynamic buffer size */
  int len = vsnprintf(buf, sizeof(buf), format, args);
  if (len >= (int)sizeof(buf)) len = sizeof(buf) - 1;
  return str_new(snap, buf, len);
}

SStr* snap_str_new_format(Snap* snap, const char* format, ...) {
  SStr* s;
  va_list args;
  va_start(args, format);
  s = snap_str_new_vformat(snap, format, args);
  va_end(args);
  return s;
}

SSym* snap_sym_new(Snap* snap, const char* name) {
  size_t len = strlen(name);
  int capacity;
  SValue* existing = snap_hash_get_str(&snap->symbols, name, len);
  SSym* s;
  if (existing) {
    return as_sym(*existing);
  }
  s = (SSym*)gc_new(snap, STYPE_SYM, sizeof(SSym) + len + 1);
  s->mark = GC_FIXED;
  s->value = create_undef();
  s->len = len;
  memcpy(s->data, name, len + 1);
  capacity = snap->symbols.hcapacity;
  snap_hash_put(&snap->symbols, create_obj(s), create_obj(s));
  if (snap->symbols.hcapacity != capacity) snap->gc_sym = 0; // Entries moved, so mark them again
  return s;
}

SKeyword* snap_key_new(Snap* snap, const char* name) {
  size_t len = strlen(name);
  SValue* existing = snap_hash_get_str(&snap->keywords, name, len);
  SKeyword* k;
  if (existing) {
    return as_key(*existing);
  }
  k = (SKeyword*)gc_new(snap, STYPE_KEY, sizeof(SKeyword) + len + 1);
  k->mark = GC_FIXED;
  k->len = len;
  memcpy(k->data, name, len + 1);
  k->id = snap->keywords.hcount;
  snap_hash_put(&snap->keywords, create_obj(k), create_obj(k));
  return k;
}

SErr* snap_err_new(Snap* snap, SValue err, SValue msg) {
  SErr* e = (SErr*)gc_new(snap, STYPE_ERR, sizeof(SErr));
  e->err = err;
  e->msg = msg;
  return e;
}

SErr* snap_err_new_format(Snap* snap, const char* err, const char* format, ...) {
  SStr* s;
  SKeyword* k;
  SErr* e;
  va_list args;
  va_start(args, format);
  s = (SStr*)anchor(snap, (SObject*)snap_str_new_vformat(snap, format, args));
  va_end(args);
  k = snap_key_new(snap, err); // Interned, so already rooted
  e = snap_err_new(snap, create_obj(k), create_obj(s));
  release(snap);
  return e;
}

SCons* snap_cons_new(Snap* snap) {
  return cons_new(snap);
}

// A vector that fits in a slot keeps its values right after itself, so
// making and freeing it don't call malloc() and free(). Pushes past cap0 go
// in segments either way.
SVec* snap_vec_new(Snap* snap, int cap) {
  bool inl = cap > 0 && sizeof(SVec) + cap * sizeof(SValue) <= GC_MAX_SLOT;
  SVec* v = (SVec*)gc_new(snap, STYPE_VEC, sizeof(SVec) + (inl ? cap * sizeof(SValue) : 0));
  v->num_segs = 0;
  v->len = 0;
  v->cap0 = cap;
  v->segs = NULL;
  if (inl) {
    v->data = vec_inline(v);
  } else {
    v->data = cap > 0 ? (SValue*)malloc(cap * sizeof(SValue)) : NULL;
    heap_add(snap, cap * sizeof(SValue));
  }
  return v;
}

void snap_vec_push(Snap* snap, SVec* vec, SValue val) {
  vec_push(snap, vec, val);
}

SMap* snap_map_new(Snap* snap) {
  SMap* m = (SMap*)gc_new(snap, STYPE_MAP, sizeof(SMap));
  m->len = m->used = m->cap = 0;
  m->entries = NULL;
  m->index = NULL;
  return m;
}

static void map_index(SMap* map, int entry) {
  int i, mask = 2 * map->cap - 1;
  for (i = snap_hash(&map->entries[2 * entry]) & mask; map->index[i] >= 0; i = (i + 1) & mask) {}
  map->index[i] = entry;
}

// Drops deleted entries and sizes the map to twice its live entries.
// Doesn't allocate GC objects, so it never collects.
void map_rebuild(Snap* snap, SMap* map) {
  int i, len = 0, cap = 4;
  SValue* entries;
  while (cap < 2 * map->len) cap *= 2;
  entries = (SValue*)malloc(2 * cap * sizeof(SValue));
  for (i = 0; i < map->used; ++i) {
    if (map->entries[2 * i].bits != SV_DELETED) {
      // A collection marking this map a chunk at a time could miss moved
      // entries, so mark them
      gc_barrier(snap, map->entries[2 * i]);
      gc_barrier(snap, map->entries[2 * i + 1]);
      entries[2 * len] = map->entries[2 * i];
      entries[2 * len + 1] = map->entries[2 * i + 1];
      len++;
    }
  }
  free(map->entries);
  free(map->index);
  snap->num_bytes_alloced -= map_bytes(map->cap);
  heap_add(snap, map_bytes(cap));
  map->entries = entries;
  map->index = (int32_t*)malloc(2 * cap * sizeof(int32_t));
  memset(map->index, -1, 2 * cap * sizeof(int32_t));
  map->cap = cap;
  map->used = len;
  for (i = 0; i < len; ++i) map_index(map, i);
}

SValue* snap_map_get(SMap* map, SValue key) {
  return map_get(map, key);
}

// Doesn't allocate GC objects, so it never collects
void snap_map_put(Snap* snap, SMap* map, SValue key, SValue val) {
  int e = map_find(map, key);
  if (e < 0) {
    if (map->used == map->cap) map_rebuild(snap, map);
    e = map->used++;
    map->entries[2 * e] = key;
    map_index(map, e);
    map->len++;
  } else {
    gc_barrier(snap, map->entries[2 * e + 1]);
  }
  map->entries[2 * e + 1] = val;
}

// A new map with src's entries in the same order, with room for extra more
// without growing. With no deleted entries and room enough at src's size,
// the entries and index are copied as they are, without hashing anything.
SMap* map_copy(Snap* snap, SMap* src, int extra) {
  SMap* m = snap_map_new(snap);
  int i, cap = src->cap;
  if (src->len + extra == 0) return m;
  if (cap < src->len + extra) {
    for (cap = 4; cap < src->len + extra; cap *= 2) {}
  }
  m->entries = (SValue*)malloc(2 * cap * sizeof(SValue));
  m->index = (int32_t*)malloc(2 * cap * sizeof(int32_t));
  m->cap = cap;
  heap_add(snap, map_bytes(cap));
  if (src->used == src->len && cap == src->cap) {
    memcpy(m->entries, src->entries, 2 * src->used * sizeof(SValue));
    memcpy(m->index, src->index, 2 * cap * sizeof(int32_t));
    m->used = m->len = src->len;
  } else {
    memset(m->index, -1, 2 * cap * sizeof(int32_t));
    for (i = 0; i < src->used; ++i) {
      if (src->entries[2 * i].bits == SV_DELETED) continue;
      m->entries[2 * m->used] = src->entries[2 * i];
      m->entries[2 * m->used + 1] = src->entries[2 * i + 1];
      map_index(m, m->used++);
    }
    m->len = m->used;
  }
  return m;
}

bool snap_map_del(Snap* snap, SMap* map, SValue key) {
  int e = map_find(map, key);
  if (e < 0) return false;
  gc_barrier(snap, map->entries[2 * e]);
  gc_barrier(snap, map->entries[2 * e + 1]);
  map->entries[2 * e] = snap_val(SV_DELETED);
  map->entries[2 * e + 1] = create_nil();
  map->len--;
  return true;
}

SClosure* snap_closure_new(Snap* snap, SCode* code) {
  return closure_new(snap, code);
}

// fields is a vector of keywords, or NULL for a built-in type
SType* type_new(Snap* snap, const char* name, SVec* fields) {
  int i, n = fields ? fields->len : 0;
  SType* t = (SType*)gc_new(snap, STYPE_TYPE, sizeof(SType) + n * sizeof(SValue));
  t->mark = GC_FIXED;
  t->is_struct = fields != NULL;
  t->num_fields = n;
  t->name = snap_sym_new(snap, name); // Interned, so it can't be collected
  t->gc = create_nil();
  for (i = 0; i < n; ++i) t->fields[i] = *snap_vec_at(fields, i);
  return t;
}

SType* snap_type_of(Snap* snap, SValue val) {
  return type_of(snap, val);
}

// Makes an instance of a struct type with the values of its fields in order.
// Doesn't run any code, so the type and values only need to be rooted.
bool struct_new(Snap* snap, SType* type, const SValue* args, int num_args, SValue* result) {
  SStruct* st;
  if (!type->is_struct) {
    *result = create_obj(snap_err_new_format(snap, "snap_type_error", "%s isn't a struct type", type->name->data));
    return false;
  }
  if (num_args != type->num_fields) {
    *result = create_obj(snap_err_new_format(snap, "snap_arity_error", "%s has %d fields",
                                             type->name->data, type->num_fields));
    return false;
  }
  st = (SStruct*)gc_new(snap, STYPE_STRUCT, sizeof(SStruct) + num_args * sizeof(SValue));
  st->stype = type;
  st->num_vals = num_args;
  if (num_args > 0) memcpy(st->vals, args, num_args * sizeof(SValue));
  if (!is_nil(type->gc) && !snap->gc_closing) *mvec_push(&snap->finobj, SObject*) = (SObject*)st;
  *result = create_obj(st);
  return true;
}


SErr* method_error(Snap* snap, SMethod* m, const SValue* args, int num_args) {
  if (num_args == 0) return snap_err_new_format(snap, "snap_arity_error", "Method %s needs an argument", m->name->data);
  return snap_err_new_format(snap, "snap_type_error", "No method %s for %s", m->name->data,
                             type_of(snap, args[0])->name->data);
}

/* Printing */

void snap_print(SValue val) {
  print_val(stdout, val);
}

// Quoted and escaped, so that it reads back as the same string
static void print_str(FILE* out, SStr* s) {
  size_t i;
  fputc('"', out);
  for (i = 0; i < s->len; ++i) {
    switch (s->data[i]) {
      case '\n': fprintf(out, "\\n"); break;
      case '\t': fprintf(out, "\\t"); break;
      case '\r': fprintf(out, "\\r"); break;
      case '\\': fprintf(out, "\\\\"); break;
      case '"': fprintf(out, "\\\""); break;
      default: fputc(s->data[i], out); break;
    }
  }
  fputc('"', out);
}

void print_val(FILE* out, SValue val) {
  int i;
  SCons* cons;
  switch (snap_type(val)) {
    case STYPE_NIL:
      fprintf(out, "nil");
      break;
    case STYPE_BOOL:
      fprintf(out, "%s", is_false(val) ? "false" : "true");
      break;
    case STYPE_INT:
      fprintf(out, "%ld", (long)as_int(val));
      break;
    case STYPE_FLOAT:
      fprintf(out, "%f", as_float(val));
      break;
    case STYPE_CFUNC:
      fprintf(out, "<cfunc %p>", (void*)as_cfunc(val));
      break;
    case STYPE_FORM:
      fprintf(out, "%s", form_name(as_form(val)));
      break;
    case STYPE_STR:
      print_str(out, as_str(val));
      break;
    case STYPE_SYM:
      fprintf(out, "%s", as_sym(val)->data);
      break;
    case STYPE_KEY:
      fprintf(out, ":%s", as_key(val)->data);
      break;
    case STYPE_ERR:
      fprintf(out, "<err ");
      print_val(out, as_err(val)->err);
      fprintf(out, " ");
      print_val(out, as_err(val)->msg);
      fprintf(out, ">");
      break;
    case STYPE_VEC:
      fprintf(out, "[");
      for (i = 0; i < as_vec(val)->len; ++i) {
        if (i > 0) fprintf(out, " ");
        print_val(out, *snap_vec_at(as_vec(val), i));
      }
      fprintf(out, "]");
      break;
    case STYPE_MAP: {
      SMap* m = as_map(val);
      bool first = true;
      fprintf(out, "{");
      for (i = 0; i < m->used; ++i) {
        if (m->entries[2 * i].bits == SV_DELETED) continue;
        if (!first) fprintf(out, " ");
        first = false;
        print_val(out, m->entries[2 * i]);
        fprintf(out, " ");
        print_val(out, m->entries[2 * i + 1]);
      }
      fprintf(out, "}");
      break;
    }
    case STYPE_CONS:
      fprintf(out, "(");
      for (cons = as_cons(val); cons != NULL; ) {
        print_val(out, cons->first);
        if (!is_cons(cons->rest)) {
          fprintf(out, " . ");
          print_val(out, cons->rest);
          break;
        }
        cons = as_cons(cons->rest);
        if (cons) fprintf(out, " ");
      }
      fprintf(out, ")");
      break;
    case STYPE_CODE:
      fprintf(out, "<fn %p>", (void*)as_obj(val));
      break;
    case STYPE_CLOSURE:
      fprintf(out, "<closure %p>", (void*)as_obj(val));
      break;
    case STYPE_TYPE:
      fprintf(out, "%s", as_type(val)->name->data);
      break;
    case STYPE_STRUCT: {
      SStruct* st = as_struct(val);
      fprintf(out, "#%s{", st->stype->name->data);
      for (i = 0; i < st->stype->num_fields; ++i) {
        if (i > 0) fprintf(out, " ");
        print_val(out, st->stype->fields[i]);
        fprintf(out, " ");
        print_val(out, st->vals[i]);
      }
      fprintf(out, "}");
      break;
    }
    case STYPE_METHOD:
      fprintf(out, "<method %s>", as_method(val)->name->data);
      break;
  }
}
