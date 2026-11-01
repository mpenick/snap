// The built-in functions
#include "snap_builtins.h"
#include "snap_vm.h"
#include "snap_compile.h"

#include <limits.h>

/* Builtins */

// Sets *result to a new error and returns false, so a builtin can panic with
// `return fail(...)`
static bool fail(Snap* snap, SValue* result, const char* err, const char* msg) {
  *result = create_obj(snap_err_new_format(snap, err, "%s", msg));
  return false;
}

static bool arity_error(Snap* snap, SValue* result) {
  return fail(snap, result, "snap_arity_error", "Invalid number of arguments");
}

static bool read_only_error(Snap* snap, SValue* result) {
  return fail(snap, result, "snap_type_error", "Can't change a quoted literal");
}

bool builtin_print(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int i;
  for (i = 0; i < num_args; ++i) {
    if (i > 0) printf(" ");
    snap_print(args[i]);
  }
  return true;
}

bool builtin_println(Snap* snap, const SValue* args, int num_args, SValue* result) {
  builtin_print(snap, args, num_args, result);
  printf("\n");
  return true;
}

bool builtin_isnil(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_bool(is_nil(args[0]));
  return true;
}

bool builtin_isvec(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_bool(is_vec(args[0]));
  return true;
}

bool builtin_ismap(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_bool(is_map(args[0]));
  return true;
}

// Type predicates, one per built-in type: (int? x) is (= (type x) Int)
#define XX(name, test) \
  static bool name(Snap* snap, const SValue* args, int num_args, SValue* result) { \
    if (num_args != 1) return arity_error(snap, result); \
    *result = create_bool(test(args[0])); \
    return true; \
  }
#define is_number(v) (is_int(v) || is_float(v))
#define is_fn_value(v) (is_cfunc(v) || is_code(v) || is_closure(v) || is_method(v))
XX(builtin_isint, is_int) XX(builtin_isfloat, is_float) XX(builtin_isnumber, is_number)
XX(builtin_isstr, is_str) XX(builtin_iskey, is_key) XX(builtin_isbool, is_bool)
XX(builtin_isfn, is_fn_value) XX(builtin_isstruct, is_struct) XX(builtin_istype, is_type)
#undef is_number
#undef is_fn_value
#undef XX

// (err <kind> <msg>) makes an error value. Unlike panic it doesn't unwind;
// it's returned like any other value (see try).
bool builtin_err(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 2) return arity_error(snap, result);
  *result = create_obj(snap_err_new(snap, args[0], args[1]));
  return true;
}

// (err? <x>) or (err? <x> <kind>)
bool builtin_iserr(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1 && num_args != 2) return arity_error(snap, result);
  *result = create_bool(is_err(args[0]) &&
                        (num_args == 1 || snap_compare(&as_err(args[0])->err, &args[1]) == 0));
  return true;
}

static bool err_arg(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  if (!is_err(args[0])) return fail(snap, result, "snap_type_error", "Expected an error");
  return true;
}

bool builtin_err_kind(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (!err_arg(snap, args, num_args, result)) return false;
  *result = as_err(args[0])->err;
  return true;
}

bool builtin_err_msg(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (!err_arg(snap, args, num_args, result)) return false;
  *result = as_err(args[0])->msg;
  return true;
}

// Builds vector literals (see compile_vec())
bool builtin_vector(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return vec_from_args(snap, args, num_args, result);
}

// (range n), (range a b) and (range a b step): a vector of the numbers from a
// (0 if not given) up to but not including b, by step (1 if not given, and it
// can be negative). Ints if every argument is an int, otherwise floats, each
// a + i * step so that the steps' rounding doesn't add up.
static bool builtin_range(Snap* snap, const SValue* args, int num_args, SValue* result) {
  const SValue* end = &args[num_args > 1 ? 1 : 0];
  bool ints = true;
  uint64_t len;
  SVec* vec;
  int i;
  if (num_args < 1 || num_args > 3) return arity_error(snap, result);
  for (i = 0; i < num_args; ++i) {
    if (!is_num_p(&args[i])) return fail(snap, result, "snap_type_error", "Expected numbers");
    if (!is_int(args[i])) ints = false;
  }
  if (ints) {
    SnapInt a = num_args > 1 ? as_int(args[0]) : 0, b = as_int(*end), step = num_args > 2 ? as_int(args[2]) : 1;
    if (step == 0) return fail(snap, result, "snap_arith_error", "range's step can't be 0");
    // Unsigned, since b - a can be past what an int holds
    if (step > 0) len = b > a ? ((uint64_t)b - (uint64_t)a + (uint64_t)step - 1) / (uint64_t)step : 0;
    else len = a > b ? ((uint64_t)a - (uint64_t)b + (uint64_t)-step - 1) / (uint64_t)-step : 0;
    if (len > INT_MAX) return fail(snap, result, "snap_arith_error", "range is too long");
    vec = snap_vec_new(snap, (int)len);
    for (i = 0; i < (int)len; ++i) vec->data[i] = create_int(a + i * step);
    vec->len = (int)len;
  } else {
    SnapFloat a = num_args > 1 ? num_p(&args[0]) : 0.0, b = num_p(end), step = num_args > 2 ? num_p(&args[2]) : 1.0;
    SnapFloat n = ceil((b - a) / step);
    if (step == 0.0) return fail(snap, result, "snap_arith_error", "range's step can't be 0");
    if (!(n > 0.0)) n = 0.0; // Also NaN
    if (n > INT_MAX) return fail(snap, result, "snap_arith_error", "range is too long");
    len = (uint64_t)n;
    // A float that can't be immediate allocates, so the vector is anchored
    // and only counts the values it has so far
    vec = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, (int)len));
    for (i = 0; i < (int)len; ++i) {
      SValue x = float_new(snap, a + i * step);
      vec->data[vec->len++] = x;
    }
    release(snap);
  }
  *result = create_obj(vec);
  return true;
}

// Checks for a vector, and an index that's in range when there is one
static bool vec_args(Snap* snap, const SValue* args, int num_args, int expected, SValue* result) {
  if (num_args != expected) return arity_error(snap, result);
  if (!is_vec(args[0])) return fail(snap, result, "snap_type_error", "Expected a vector");
  if (expected > 1) {
    if (!is_int(args[1])) return fail(snap, result, "snap_type_error", "Expected an integer index");
    if (as_int(args[1]) < 0 || as_int(args[1]) >= as_vec(args[0])->len) {
      return fail(snap, result, "snap_index_error", "Index out of range");
    }
  }
  return true;
}

// Builds map literals (see compile_map()) from key/value pairs
bool builtin_map(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int i;
  SMap* map = snap_map_new(snap);
  for (i = 0; i < num_args; i += 2) {
    snap_map_put(snap, map, args[i], args[i + 1]);
  }
  *result = create_obj(map);
  return true;
}

bool builtin_count(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  if (is_nil(args[0])) {
    *result = create_int(0); // nil counts as an empty list
  } else if (is_cons(args[0])) {
    *result = create_int(list_length(as_cons(args[0])));
  } else if (is_vec(args[0])) {
    *result = create_int(as_vec(args[0])->len);
  } else if (is_map(args[0])) {
    *result = create_int(as_map(args[0])->len);
  } else {
    return fail(snap, result, "snap_type_error", "Expected a vector, map or list");
  }
  return true;
}

// (nth <vec> <index>) or (nth <list> <index>)
bool builtin_nth(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args == 2 && is_cons(args[0]) && is_int(args[1])) {
    if (as_int(args[1]) < 0 || as_int(args[1]) >= list_length(as_cons(args[0]))) {
      return fail(snap, result, "snap_index_error", "Index out of range");
    }
    *result = list_nth(as_cons(args[0]), as_int(args[1]));
    return true;
  }
  if (num_args == 2 && is_map(args[0]) && is_int(args[1])) {
    // A map's i-th entry in insertion order, as [k v]
    SValue* e = map_entry_at(snap, as_map(args[0]), as_int(args[1]));
    SVec* vec;
    if (!e) return fail(snap, result, "snap_index_error", "Index out of range");
    vec = snap_vec_new(snap, 2); // Doesn't move the entries
    vec->data[0] = e[0];
    vec->data[1] = e[1];
    vec->len = 2;
    *result = create_obj(vec);
    return true;
  }
  if (!vec_args(snap, args, num_args, 2, result)) return false;
  *result = *snap_vec_at(as_vec(args[0]), as_int(args[1]));
  return true;
}

static bool field_error(Snap* snap, const SValue* args, SValue* result) {
  *result = create_obj(snap_err_new_format(snap, "snap_key_error", "%s has no field :%s",
                                           as_struct(args[0])->stype->name->data,
                                           is_key(args[1]) ? as_key(args[1])->data : "(not a keyword)"));
  return false;
}

// (assoc coll k v ...) is a copy of a map, struct or vector with those keys,
// fields or indexes set (index len appends). (assoc nil k v ...) is a new map.
// It copies the whole collection, so put! is the way to build one up.
static bool builtin_assoc(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int i;
  if (num_args < 3 || num_args % 2 == 0) return arity_error(snap, result);
  if (is_nil(args[0]) || is_map(args[0])) {
    SMap* m = is_nil(args[0]) ? snap_map_new(snap) : map_copy(snap, as_map(args[0]), (num_args - 1) / 2);
    for (i = 1; i < num_args; i += 2) snap_map_put(snap, m, args[i], args[i + 1]);
    *result = create_obj(m);
  } else if (is_struct(args[0])) {
    SStruct* src = as_struct(args[0]), * st;
    for (i = 1; i < num_args; i += 2) {
      if (field_index(src->stype, args[i]) < 0) {
        SValue at[2] = { args[0], args[i] };
        return field_error(snap, at, result);
      }
    }
    // Made as the type makes them, so a finalizer (see defgc) runs for it too
    if (!struct_new(snap, src->stype, src->vals, src->num_vals, result)) return false;
    st = as_struct(*result);
    for (i = 1; i < num_args; i += 2) st->vals[field_index(st->stype, args[i])] = args[i + 1];
  } else if (is_vec(args[0])) {
    SVec* src = as_vec(args[0]), * v;
    int len = src->len;
    for (i = 1; i < num_args; i += 2) {
      if (!is_int(args[i])) return fail(snap, result, "snap_type_error", "Expected an index");
      if (as_int(args[i]) < 0 || as_int(args[i]) > len) return fail(snap, result, "snap_index_error", "Index out of range");
      if (as_int(args[i]) == len) len++; // Each append makes room for the next
    }
    v = snap_vec_new(snap, len);
    for (i = 0; i < src->len; ++i) v->data[i] = *snap_vec_at(src, i);
    v->len = src->len;
    for (i = 1; i < num_args; i += 2) {
      if (as_int(args[i]) == v->len) v->data[v->len++] = args[i + 1];
      else v->data[as_int(args[i])] = args[i + 1];
    }
    *result = create_obj(v);
  } else {
    return fail(snap, result, "snap_type_error", "Expected a map, struct, vector or nil");
  }
  return true;
}

// (dissoc m k ...) is a copy of a map without those keys
static bool builtin_dissoc(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SMap* m;
  int i;
  if (num_args < 1) return arity_error(snap, result);
  if (is_nil(args[0])) {
    *result = create_nil();
    return true;
  }
  if (!is_map(args[0])) return fail(snap, result, "snap_type_error", "Expected a map or nil");
  m = map_copy(snap, as_map(args[0]), 0);
  for (i = 1; i < num_args; ++i) snap_map_del(snap, m, args[i]);
  *result = create_obj(m);
  return true;
}

// (merge m ...) is a new map with every map's entries, later ones winning.
// nil arguments are skipped, and with only those it's nil.
static bool builtin_merge(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int i, j, first = -1, extra = 0;
  SMap* m;
  for (i = 0; i < num_args; ++i) {
    if (is_nil(args[i])) continue;
    if (!is_map(args[i])) return fail(snap, result, "snap_type_error", "Expected maps or nil");
    if (first < 0) first = i;
    else extra += as_map(args[i])->len;
  }
  if (first < 0) {
    *result = create_nil();
    return true;
  }
  m = map_copy(snap, as_map(args[first]), extra);
  for (i = first + 1; i < num_args; ++i) {
    SMap* src;
    if (is_nil(args[i])) continue;
    src = as_map(args[i]);
    for (j = 0; j < src->used; ++j) {
      if (src->entries[2 * j].bits != SV_DELETED) snap_map_put(snap, m, src->entries[2 * j], src->entries[2 * j + 1]);
    }
  }
  *result = create_obj(m);
  return true;
}

// (put! <vec> <index> <val>), (put! <map> <key> <val>) or
// (put! <struct> <field> <val>) returns the vector, map or struct
bool builtin_put(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SValue* val;
  if (num_args == 3 && is_struct(args[0])) {
    if (!(val = struct_field(as_struct(args[0]), args[1]))) return field_error(snap, args, result);
    gc_barrier(snap, *val);
    *val = args[2];
    *result = args[0];
    return true;
  }
  if (num_args == 3 && is_map(args[0])) {
    if (as_obj(args[0])->read_only) return read_only_error(snap, result);
    snap_map_put(snap, as_map(args[0]), args[1], args[2]);
    *result = args[0];
    return true;
  }
  if (!vec_args(snap, args, num_args, 3, result)) return false;
  if (as_obj(args[0])->read_only) return read_only_error(snap, result);
  val = snap_vec_at(as_vec(args[0]), as_int(args[1]));
  gc_barrier(snap, *val);
  *val = args[2];
  *result = args[0];
  return true;
}

// (push! <vec> <val>...) appends to the end and returns the vector
bool builtin_push(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int i;
  if (num_args < 2) return arity_error(snap, result);
  if (!is_vec(args[0])) return fail(snap, result, "snap_type_error", "Expected a vector");
  if (as_obj(args[0])->read_only) return read_only_error(snap, result);
  for (i = 1; i < num_args; ++i) {
    vec_push(snap, as_vec(args[0]), args[i]);
  }
  *result = args[0];
  return true;
}

// (pop! <vec>) removes and returns the last value
bool builtin_pop(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SVec* vec;
  if (!vec_args(snap, args, num_args, 1, result)) return false;
  vec = as_vec(args[0]);
  if (vec->read_only) return read_only_error(snap, result);
  if (vec->len == 0) return fail(snap, result, "snap_index_error", "Vector is empty");
  *result = *snap_vec_at(vec, --vec->len);
  gc_barrier(snap, *result);
  vec_shrink(snap, vec);
  return true;
}

// (get <map> <key>) or (get <map> <key> <default>). The default is nil.
// (get <struct> <field>) panics if the struct doesn't have the field.
bool builtin_get(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SValue* val;
  if (num_args != 2 && num_args != 3) return arity_error(snap, result);
  if (is_struct(args[0])) {
    if (!(val = struct_field(as_struct(args[0]), args[1]))) return field_error(snap, args, result);
    *result = *val;
    return true;
  }
  if (!is_map(args[0])) return fail(snap, result, "snap_type_error", "Expected a map");
  val = map_get(as_map(args[0]), args[1]);
  *result = val ? *val : (num_args == 3 ? args[2] : create_nil());
  return true;
}

// (del! <map> <key>) returns the map, whether or not the key was there
bool builtin_del(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 2) return arity_error(snap, result);
  if (!is_map(args[0])) return fail(snap, result, "snap_type_error", "Expected a map");
  if (as_obj(args[0])->read_only) return read_only_error(snap, result);
  snap_map_del(snap, as_map(args[0]), args[1]);
  *result = args[0];
  return true;
}

// The keys (offset 0) or values (offset 1) as a vector, in insertion order
static bool map_entries(Snap* snap, const SValue* args, int num_args, int offset, SValue* result) {
  SMap* map;
  SVec* vec;
  int i;
  if (num_args != 1) return arity_error(snap, result);
  if (!is_map(args[0])) return fail(snap, result, "snap_type_error", "Expected a map");
  map = as_map(args[0]);
  vec = snap_vec_new(snap, map->len);
  for (i = 0; i < map->used; ++i) {
    if (map->entries[2 * i].bits != SV_DELETED) vec->data[vec->len++] = map->entries[2 * i + offset];
  }
  *result = create_obj(vec);
  return true;
}

// (str <str>...) joins strings
// (str x...) joins strings, and other values as they print
bool builtin_str(Snap* snap, const SValue* args, int num_args, SValue* result) {
  char* buf = NULL;
  size_t len = 0;
  FILE* out = open_memstream(&buf, &len);
  SStr* s;
  int i;
  for (i = 0; i < num_args; ++i) {
    if (is_str(args[i])) fwrite(as_str(args[i])->data, 1, as_str(args[i])->len, out);
    else print_val(out, args[i]);
  }
  fclose(out);
  s = (SStr*)gc_new(snap, STYPE_STR, sizeof(SStr) + len + 1);
  memcpy(s->data, buf, len + 1);
  s->len = len;
  free(buf);
  *result = create_obj(s);
  return true;
}

// (format <fmt> <arg>...) is a string made as C's printf makes one: each
// %[flags][width][.precision]conversion in fmt takes the next argument.
// d i o x X c take an integer (a float is truncated), f F e E g G a number,
// s anything (a string as it is, anything else as str prints it), and %%
// is a %. A * for the width or precision takes it from the next argument.
// There are no length modifiers (l, h, ...): snap's integers are all 64 bit.
bool builtin_format(Snap* snap, const SValue* args, int num_args, SValue* result) {
  const char* f;
  char* buf = NULL;
  size_t len = 0;
  FILE* out;
  SStr* s;
  int next = 1;
  const char* err = NULL;
  if (num_args < 1) return arity_error(snap, result);
  if (!is_str(args[0])) return fail(snap, result, "snap_type_error", "Expected a format string");
  f = as_str(args[0])->data;
  out = open_memstream(&buf, &len);
  while (*f && !err) {
    char spec[64];
    int n = 0, star[2], num_stars = 0, k;
    char conv;
    if (*f != '%') {
      fputc(*f++, out);
      continue;
    }
    if (f[1] == '%') {
      fputc('%', out);
      f += 2;
      continue;
    }
    spec[n++] = *f++;
    while (*f && strchr("-+ #0", *f) && n < 40) spec[n++] = *f++;
    for (k = 0; k < 2 && !err; ++k) {
      if (k == 1) {
        if (*f != '.') break;
        spec[n++] = *f++;
      }
      if (*f == '*') {
        SValue a;
        if (next >= num_args) { err = "Too few arguments for the format"; break; }
        a = args[next++];
        if (!is_int(a)) { err = "Expected an integer for a * in the format"; break; }
        star[num_stars++] = (int)as_int(a);
        spec[n++] = *f++;
      } else {
        while (*f >= '0' && *f <= '9' && n < 50) spec[n++] = *f++;
      }
    }
    if (err) break;
    conv = *f;
    if (!conv || !strchr("diouxXcfFeEgGs", conv)) { err = "Expected a conversion (d i o u x X c f F e E g G s) in the format"; break; }
    ++f;
    if (next >= num_args) { err = "Too few arguments for the format"; break; }
    {
      SValue a = args[next++];
      if (strchr("diouxXc", conv)) {
        long long v;
        if (is_int(a)) v = (long long)as_int(a);
        else if (is_float(a)) v = (long long)as_float(a);
        else { err = "Expected a number for an integer conversion in the format"; break; }
        if (conv == 'c') {
          spec[n++] = 'c';
          spec[n] = 0;
          if (num_stars == 2) fprintf(out, spec, star[0], star[1], (int)v);
          else if (num_stars == 1) fprintf(out, spec, star[0], (int)v);
          else fprintf(out, spec, (int)v);
        } else {
          spec[n++] = 'l';
          spec[n++] = 'l';
          spec[n++] = conv;
          spec[n] = 0;
          if (num_stars == 2) fprintf(out, spec, star[0], star[1], v);
          else if (num_stars == 1) fprintf(out, spec, star[0], v);
          else fprintf(out, spec, v);
        }
      } else if (conv == 's') {
        char* sb = NULL;
        size_t sl = 0;
        const char* str;
        if (is_str(a)) str = as_str(a)->data;
        else {
          FILE* so = open_memstream(&sb, &sl);
          print_val(so, a);
          fclose(so);
          str = sb;
        }
        spec[n++] = 's';
        spec[n] = 0;
        if (num_stars == 2) fprintf(out, spec, star[0], star[1], str);
        else if (num_stars == 1) fprintf(out, spec, star[0], str);
        else fprintf(out, spec, str);
        free(sb);
      } else {
        double v;
        if (is_int(a)) v = (double)as_int(a);
        else if (is_float(a)) v = as_float(a);
        else { err = "Expected a number for a float conversion in the format"; break; }
        spec[n++] = conv;
        spec[n] = 0;
        if (num_stars == 2) fprintf(out, spec, star[0], star[1], v);
        else if (num_stars == 1) fprintf(out, spec, star[0], v);
        else fprintf(out, spec, v);
      }
    }
  }
  if (!err && next < num_args) err = "Too many arguments for the format";
  fclose(out);
  if (err) {
    free(buf);
    return fail(snap, result, "snap_type_error", err);
  }
  s = (SStr*)gc_new(snap, STYPE_STR, sizeof(SStr) + len + 1);
  memcpy(s->data, buf, len + 1);
  s->len = len;
  free(buf);
  *result = create_obj(s);
  return true;
}

// (name <keyword or symbol>) is its name as a string, without a keyword's
// colon; a string is itself
bool builtin_name(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  if (is_str(args[0])) *result = args[0];
  else if (is_key(args[0])) *result = create_obj(snap_str_new(snap, as_key(args[0])->data));
  else if (is_sym(args[0])) *result = create_obj(snap_str_new(snap, as_sym(args[0])->data));
  else return fail(snap, result, "snap_type_error", "Expected a keyword, symbol or string");
  return true;
}

// (compile <str>) compiles code into a function without arguments, or returns
// an error if it doesn't parse or compile, or nil if there's no code
// (compile <src>) or (compile <src> <name>), name being the file its places
// say (see err-where)
bool builtin_compile(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1 && num_args != 2) return arity_error(snap, result);
  if (!is_str(args[0]) || (num_args == 2 && !is_str(args[1]))) {
    return fail(snap, result, "snap_type_error", "Expected (compile <src> [<name>]) with strings");
  }
  *result = snap_compile_as(snap, as_str(args[0])->data, num_args == 2 ? args[1] : create_nil());
  return true;
}

// (gc-step <work>) does about <work> units of collection, and returns true
// if that finished a collection
bool builtin_gc_step(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  if (!is_int(args[0]) || as_int(args[0]) < 0) return fail(snap, result, "snap_type_error", "Expected a non-negative integer");
  *result = create_bool(snap_gc_step(snap, as_int(args[0])));
  return true;
}

static int compare_doubles(const void* a, const void* b) {
  double x = *(const double*)a, y = *(const double*)b;
  return (x > y) - (x < y);
}

// Sorts the latest pauses into out. Returns how many there are.
static size_t gc_recent_sorted(const SnapGCStats* stats, double* out) {
  size_t n = stats->steps < SNAP_GC_RECENT ? stats->steps : SNAP_GC_RECENT;
  memcpy(out, stats->recent, n * sizeof(double));
  qsort(out, n, sizeof(double), compare_doubles);
  return n;
}

// The pth percentile (0 to 1) of n sorted times, between the two nearest
static double percentile(const double* sorted, size_t n, double p) {
  double pos = p * (double)(n - 1);
  size_t i = (size_t)pos;
  if (n == 0) return 0;
  if (i + 1 >= n) return sorted[n - 1];
  return sorted[i] + (sorted[i + 1] - sorted[i]) * (pos - (double)i);
}

// (gc-stats) is a map of what the collector has done, and the heap now
// (clock) is seconds of wall time, from an arbitrary start that doesn't
// move: for timing. (cpu-time) is the seconds of CPU the process has used.
static bool seconds(Snap* snap, clockid_t id, int num_args, SValue* result) {
  struct timespec ts;
  if (num_args != 0) return arity_error(snap, result);
  clock_gettime(id, &ts);
  *result = float_new(snap, (double)ts.tv_sec + ts.tv_nsec * 1e-9);
  return true;
}

bool builtin_clock(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return seconds(snap, CLOCK_MONOTONIC, num_args, result);
}

bool builtin_cpu_time(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return seconds(snap, CLOCK_PROCESS_CPUTIME_ID, num_args, result);
}

// (alloc-profile! true) starts the allocation profile afresh and
// (alloc-profile! false) pauses it
bool builtin_alloc_profile_set(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  if (!is_falsy(args[0]) && !snap->alloc_profile) {
    free(snap->alloc_sites);
    snap->alloc_sites = NULL;
    snap->alloc_sites_cap = 0;
    snap->alloc_sites_count = 0;
  }
  snap->alloc_profile = !is_falsy(args[0]);
  *result = create_nil();
  return true;
}

// The name of the global function (or method) code is, or is in, as "f",
// "f/fn" for a function made in f, or "m[Type]" for a method's
static bool code_name_in(SCode* c, SCode* target, const char* prefix, char* out, size_t n, int depth) {
  int i;
  if (c == target) {
    snprintf(out, n, "%s", prefix);
    return true;
  }
  if (depth >= 8 || depth < 0) return false; // depth -1: only c itself
  for (i = 0; i < c->num_constants; ++i) {
    if (is_code(c->constants[i])) {
      char inner[256];
      snprintf(inner, sizeof(inner), "%.250s/fn", prefix);
      if (code_name_in(as_code(c->constants[i]), target, inner, out, n, depth + 1)) return true;
    }
  }
  return false;
}

static bool fn_name_in(SValue fn, SCode* target, const char* prefix, char* out, size_t n, int depth) {
  if (is_closure(fn)) return code_name_in(as_closure(fn)->code, target, prefix, out, n, depth);
  if (is_code(fn)) return code_name_in(as_code(fn), target, prefix, out, n, depth);
  return false;
}

static void code_name(Snap* snap, SCode* target, char* out, size_t n) {
  int i, j, pass;
  if (target == NULL) {
    snprintf(out, n, "<outside snap code>");
    return;
  }
  if (target == FREED_CODE) {
    snprintf(out, n, "<freed>");
    return;
  }
  // First the globals' functions themselves, then the functions in them: a
  // function inlined in another (see inline_global) is among its constants
  for (pass = 0; pass < 2; ++pass) {
    for (i = 0; i < snap->symbols.hcapacity; ++i) {
      SValue key = snap->symbols.hentries[i].hkey, val;
      if (key.bits == snap->symbols.hempty.bits || key.bits == snap->symbols.hdeleted.bits) continue;
      val = as_sym(key)->value;
      if (fn_name_in(val, target, as_sym(key)->data, out, n, pass - 1)) return;
      if (is_method(val)) {
        SMap* table = as_method(val)->table;
        for (j = 0; j < table->used; ++j) {
          char prefix[256];
          if (table->entries[2 * j].bits == SV_DELETED || !is_type(table->entries[2 * j])) continue;
          snprintf(prefix, sizeof(prefix), "%s[%s]", as_sym(key)->data, as_type(table->entries[2 * j])->name->data);
          if (fn_name_in(table->entries[2 * j + 1], target, prefix, out, n, pass - 1)) return;
        }
      }
    }
  }
  snprintf(out, n, "<anonymous>"); // Top-level code, or a function only data holds
}

// The line run instruction at is in (see SCode), or NULL
static SnapLineRun* line_run(SCode* code, int at) {
  int lo = 0, hi = code->num_lines - 1;
  SnapLineRun* found = NULL;
  while (lo <= hi) {
    int mid = (lo + hi) / 2;
    if (code->lines[mid].pc <= at) {
      found = &code->lines[mid];
      lo = mid + 1;
    } else {
      hi = mid - 1;
    }
  }
  return found ? found : (code->num_lines > 0 ? &code->lines[0] : NULL);
}

static SCode* value_code(SValue fn) {
  return is_closure(fn) ? as_closure(fn)->code : is_code(fn) ? as_code(fn) : NULL;
}

// Adds [file line name] (and :inlined if it is) to out
static void add_place(Snap* snap, SVec* out, SValue file, int line, SCode* code, bool inlined) {
  SVec* place = snap_vec_new(snap, 4);
  char name[256];
  vec_push(snap, out, create_obj(place)); // So it's rooted
  vec_push(snap, place, file);
  vec_push(snap, place, create_int(line));
  if (code) code_name(snap, code, name, sizeof(name));
  vec_push(snap, place, code && strcmp(name, "<anonymous>") != 0 ? create_obj(snap_str_new_format(snap, "%s", name))
                                                                : create_nil());
  if (inlined) vec_push(snap, place, create_obj(snap_key_new(snap, "inlined")));
}

SValue snap_err_where(Snap* snap, SValue err) {
  SVec *where, *out;
  int i, j;
  if (!is_err(err) || !is_vec(as_err(err)->where)) return create_nil();
  where = as_vec(as_err(err)->where);
  out = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, where->len / 2));
  for (i = 0; i + 1 < where->len; i += 2) {
    SValue x = *snap_vec_at(where, i);
    int n = (int)as_int(*snap_vec_at(where, i + 1));
    if (is_key(x)) { // :skipped and how many
      SVec* gap = snap_vec_new(snap, 2);
      vec_push(snap, out, create_obj(gap));
      vec_push(snap, gap, x);
      vec_push(snap, gap, create_int(n));
    } else if (is_code(x)) {
      SCode* code = as_code(x);
      SnapLineRun* run = line_run(code, n);
      // Inlined functions, innermost first, at the line their code starts on
      for (j = 1; run && j >= 0; --j) {
        SCode* in = run->inlined[j] >= 0 ? value_code(code->constants[run->inlined[j]]) : NULL;
        SnapLineRun* first = in ? line_run(in, 0) : NULL;
        if (in) add_place(snap, out, in->file, first ? first->line : 0, in, true);
      }
      add_place(snap, out, code->file, run ? run->line : 0, code, false);
    } else {
      add_place(snap, out, x, n, NULL, false);
    }
  }
  release(snap);
  return create_obj(out);
}

// Appends printf output to a malloc()ed buffer
typedef struct {
  char* data;
  size_t len, cap;
} SnapBuf;

static void buf_printf(SnapBuf* b, const char* format, ...) {
  va_list args;
  int n;
  va_start(args, format);
  n = vsnprintf(NULL, 0, format, args);
  va_end(args);
  if (b->len + n + 1 > b->cap) {
    b->cap = 2 * (b->len + n + 1);
    b->data = (char*)realloc(b->data, b->cap);
  }
  va_start(args, format);
  vsnprintf(b->data + b->len, n + 1, format, args);
  va_end(args);
  b->len += n;
}

SValue snap_err_trace(Snap* snap, SValue err) {
  SnapBuf b = { NULL, 0, 0 };
  SValue where, result;
  int i;
  buf_printf(&b, "%s", "");
  if (is_err(err)) {
    where = snap_pin(snap, snap_err_where(snap, err));
    for (i = 0; is_vec(where) && i < as_vec(where)->len; ++i) {
      SVec* place = as_vec(*snap_vec_at(as_vec(where), i));
      if (place->len == 2) { // [:skipped n]
        buf_printf(&b, "\n  ... %d more", (int)as_int(*snap_vec_at(place, 1)));
      } else {
        SValue file = *snap_vec_at(place, 0), name = *snap_vec_at(place, 2);
        SnapInt line = as_int(*snap_vec_at(place, 1));
        buf_printf(&b, "\n  at %s", is_str(file) ? as_str(file)->data : "<string>");
        if (line > 0) buf_printf(&b, ":%d", (int)line);
        if (is_str(name)) buf_printf(&b, " in %s", as_str(name)->data);
        if (place->len > 3) buf_printf(&b, " (inlined)");
      }
    }
    snap_unpin(snap, where);
  }
  result = create_obj(snap_str_new(snap, b.data));
  free(b.data);
  return result;
}

void snap_print_err(Snap* snap, FILE* out, SValue value) {
  print_val(out, value);
  if (is_err(value)) {
    snap_pin(snap, value);
    fputs(as_str(snap_err_trace(snap, value))->data, out);
    snap_unpin(snap, value);
  }
}

// (err-where <err>): see snap_err_where()
static bool builtin_err_where(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (!err_arg(snap, args, num_args, result)) return false;
  *result = snap_err_where(snap, args[0]);
  return true;
}

// (err-trace <err>): see snap_err_trace()
static bool builtin_err_trace(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (!err_arg(snap, args, num_args, result)) return false;
  *result = snap_err_trace(snap, args[0]);
  return true;
}

static const char* kind_name(uint8_t type) {
  if (type == STYPE_CONS) return "list";
  switch (type) {
#define XX(type, id, name) case type: return name;
    STYPE_MAPPING(XX)
#undef XX
    default: return "other";
  }
}

typedef struct {
  char name[256];
  uint8_t type;
  uint64_t count;
  uint64_t bytes;
} ProfileRow;

// Most bytes first, then by name and kind, so the order doesn't depend on
// where things are in memory
static int row_by_bytes(const void* a, const void* b) {
  const ProfileRow* x = (const ProfileRow*)a;
  const ProfileRow* y = (const ProfileRow*)b;
  int c;
  if (x->bytes != y->bytes) return x->bytes < y->bytes ? 1 : -1;
  if ((c = strcmp(x->name, y->name)) != 0) return c;
  return strcmp(kind_name(x->type), kind_name(y->type));
}

// (alloc-profile) stops the profile and returns what it saw, most bytes
// first, as [function kind count bytes] rows, then clears it
bool builtin_alloc_profile(Snap* snap, const SValue* args, int num_args, SValue* result) {
  ProfileRow* rows;
  SVec* out;
  int i, j, n = 0;
  if (num_args != 0) return arity_error(snap, result);
  snap->alloc_profile = false;
  rows = (ProfileRow*)malloc((snap->alloc_sites_count + 1) * sizeof(ProfileRow));
  for (i = 0; i < snap->alloc_sites_cap; ++i) {
    SnapAllocSite* site = &snap->alloc_sites[i];
    char name[256];
    if (!site->count) continue;
    code_name(snap, site->code, name, sizeof(name));
    // Rows with the same name (two functions made in one) are added up
    for (j = 0; j < n && !(rows[j].type == site->type && strcmp(rows[j].name, name) == 0); ++j) {}
    if (j == n) {
      memcpy(rows[n].name, name, sizeof(name));
      rows[n].type = site->type;
      rows[n].count = rows[n].bytes = 0;
      n++;
    }
    rows[j].count += site->count;
    rows[j].bytes += site->bytes;
  }
  free(snap->alloc_sites);
  snap->alloc_sites = NULL;
  snap->alloc_sites_cap = 0;
  snap->alloc_sites_count = 0;
  qsort(rows, n, sizeof(ProfileRow), row_by_bytes);
  out = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, n));
  for (i = 0; i < n; ++i) {
    SVec* row = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, 4));
    vec_push(snap, row, create_obj(snap_str_new(snap, rows[i].name)));
    vec_push(snap, row, create_obj(snap_key_new(snap, kind_name(rows[i].type))));
    vec_push(snap, row, create_int((SnapInt)rows[i].count));
    vec_push(snap, row, create_int((SnapInt)rows[i].bytes));
    vec_push(snap, out, create_obj(row));
    release(snap);
  }
  free(rows);
  release(snap);
  *result = create_obj(out);
  return true;
}

bool builtin_gc_stats(Snap* snap, const SValue* args, int num_args, SValue* result) {
  static const char* phases[] = { "idle", "mark", "sweep" };
  const SnapGCStats* stats = &snap->gc_stats;
  double sorted[SNAP_GC_RECENT];
  size_t n = gc_recent_sorted(stats, sorted);
  SMap* m;
  if (num_args != 0) return arity_error(snap, result);
  m = (SMap*)anchor(snap, (SObject*)snap_map_new(snap));
#define PUT(name, val) snap_map_put(snap, m, create_obj(snap_key_new(snap, name)), val)
  PUT("heap", create_int((SnapInt)snap->num_bytes_alloced));
  PUT("next-gc", create_int((SnapInt)snap->next_gc));
  PUT("phase", create_obj(snap_key_new(snap, phases[snap->gc_phase])));
  PUT("collections", create_int((SnapInt)stats->collections));
  PUT("steps", create_int((SnapInt)stats->steps));
  PUT("allocated", create_int((SnapInt)stats->allocated));
  PUT("pause-total", float_new(snap, stats->pause_total));
  PUT("pause-max", float_new(snap, stats->pause_max));
  PUT("pause-mean", float_new(snap, stats->steps ? stats->pause_total / stats->steps : 0));
  PUT("pause-median", float_new(snap, percentile(sorted, n, 0.5)));
  PUT("pause-p75", float_new(snap, percentile(sorted, n, 0.75)));
  PUT("pause-p90", float_new(snap, percentile(sorted, n, 0.9)));
  PUT("pause-p99", float_new(snap, percentile(sorted, n, 0.99)));
#undef PUT
  release(snap);
  *result = create_obj(m);
  return true;
}

bool builtin_keys(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return map_entries(snap, args, num_args, 0, result);
}

bool builtin_vals(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return map_entries(snap, args, num_args, 1, result);
}

/* Lists, mostly for macros. nil counts as an empty list. */

static bool list_arg(Snap* snap, SValue val, SCons** list, SValue* result) {
  if (is_nil(val)) {
    *list = NULL;
    return true;
  }
  if (!is_cons(val)) return fail(snap, result, "snap_type_error", "Expected a list");
  *list = as_cons(val);
  return true;
}

bool builtin_list(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int i;
  SnapList list = { NULL, NULL };
  for (i = 0; i < num_args; ++i) *list_add(snap, &list) = args[i];
  *result = list_done(snap, &list);
  return true;
}

// (cons x list) is list with x in front
bool builtin_cons(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SCons* list;
  SCons* cell;
  if (num_args != 2) return arity_error(snap, result);
  if (!list_arg(snap, args[1], &list, result)) return false;
  cell = cons_new(snap);
  cell->first = args[0];
  cell->rest = list ? args[1] : create_empty();
  *result = create_obj(cell);
  return true;
}

// The first value of a list, or nil if it's empty
bool builtin_first(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SCons* list;
  if (num_args != 1) return arity_error(snap, result);
  if (!list_arg(snap, args[0], &list, result)) return false;
  *result = list ? list->first : create_nil();
  return true;
}

// A list without its first value: () if it's empty
bool builtin_rest(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SCons* list;
  if (num_args != 1) return arity_error(snap, result);
  if (!list_arg(snap, args[0], &list, result)) return false;
  *result = list ? list->rest : create_empty();
  return true;
}

// (next list) is the list after its first value, or nil if that's empty, so
// (if xs ...) can tell when a walk down a list is done
bool builtin_next(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SCons* list;
  if (num_args != 1) return arity_error(snap, result);
  if (!list_arg(snap, args[0], &list, result)) return false;
  *result = list && !is_empty(list->rest) ? list->rest : create_nil();
  return true;
}

// (nthnext coll i) is a list of the values of a list or vector from index i
// on, or nil if there are none
bool builtin_nthnext(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SnapList out = { NULL, NULL };
  SnapInt i, n;
  if (num_args != 2) return arity_error(snap, result);
  if (!is_int(args[1]) || as_int(args[1]) < 0) return fail(snap, result, "snap_type_error", "Expected an index of 0 or more");
  n = as_int(args[1]);
  if (is_vec(args[0])) {
    SVec* v = as_vec(args[0]);
    for (i = n; i < v->len; ++i) *list_add(snap, &out) = *snap_vec_at(v, (int)i);
    *result = out.head ? list_done(snap, &out) : create_nil();
  } else if (is_nil(args[0]) || is_cons(args[0])) {
    SCons* list = is_nil(args[0]) ? NULL : as_cons(args[0]);
    for (i = 0; list && i < n; ++i) list = as_cons(list->rest);
    *result = list ? create_obj(list) : create_nil();
  } else {
    return fail(snap, result, "snap_type_error", "Expected a vector or list");
  }
  return true;
}

// (concat a b...) is a new list of the values in the lists or vectors
bool builtin_concat(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int i, j;
  SnapList list = { NULL, NULL };
  SCons* curr;
  for (i = 0; i < num_args; ++i) {
    if (!is_vec(args[i]) && !list_arg(snap, args[i], &curr, result)) return false;
  }
  for (i = 0; i < num_args; ++i) {
    if (is_vec(args[i])) {
      for (j = 0; j < as_vec(args[i])->len; ++j) {
        *list_add(snap, &list) = *snap_vec_at(as_vec(args[i]), j);
      }
    } else {
      for (curr = is_nil(args[i]) ? NULL : as_cons(args[i]); curr; curr = as_cons(curr->rest)) {
        *list_add(snap, &list) = curr->first;
      }
    }
  }
  *result = list_done(snap, &list);
  return true;
}

// (vec list) is a new vector of the values in a list
bool builtin_vec(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SCons* list;
  SVec* vec;
  if (num_args != 1) return arity_error(snap, result);
  if (is_map(args[0])) {
    // A map's entries as [k v] in order. Each pair allocates, so the vector
    // is anchored and only counts the pairs it has so far.
    SMap* m = as_map(args[0]);
    int i;
    vec = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, m->len));
    for (i = 0; i < m->used; ++i) {
      SVec* pair;
      if (m->entries[2 * i].bits == SV_DELETED) continue;
      pair = snap_vec_new(snap, 2);
      pair->data[0] = m->entries[2 * i];
      pair->data[1] = m->entries[2 * i + 1];
      pair->len = 2;
      vec->data[vec->len++] = create_obj(pair);
    }
    release(snap);
    *result = create_obj(vec);
    return true;
  }
  if (is_vec(args[0])) {
    // A copy, so a function can take a list or a vector alike
    SVec* src = as_vec(args[0]);
    int i, n = src->len;
    vec = snap_vec_new(snap, n); // The only allocation
    for (i = 0; i < n; ++i) vec->data[i] = *snap_vec_at(src, i);
    vec->len = n;
    *result = create_obj(vec);
    return true;
  }
  if (!is_cons(args[0]) && !is_nil(args[0])) return fail(snap, result, "snap_type_error", "Expected a list, vector or map");
  if (!list_arg(snap, args[0], &list, result)) return false;
  vec = snap_vec_new(snap, list_length(list)); // The only allocation
  for (; list; list = as_cons(list->rest)) vec_push(snap, vec, list->first);
  *result = create_obj(vec);
  return true;
}

bool builtin_islist(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_bool(is_cons(args[0]));
  return true;
}

bool builtin_issym(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_bool(is_sym(args[0]));
  return true;
}

/* Macros */

// Marks the function from defmacro (see compile_defmacro())
bool builtin_make_macro(Snap* snap, const SValue* args, int num_args, SValue* result) {
  as_obj(args[0])->macro = true;
  *result = args[0];
  return true;
}

// (gensym) or (gensym <prefix>) makes a new symbol, #g1, #g2..., that can't
// be the same as any in the code, for a macro's own variables
bool builtin_gensym(Snap* snap, const SValue* args, int num_args, SValue* result) {
  char name[128];
  if (num_args > 1 || (num_args == 1 && !is_str(args[0]))) {
    return fail(snap, result, "snap_type_error", "Expected (gensym <prefix>)");
  }
  snprintf(name, sizeof(name), "#%.100s%d", num_args == 1 ? as_str(args[0])->data : "g",
           ++snap->num_gensyms);
  *result = create_obj(snap_sym_new(snap, name)); // Interned, so it lives forever
  return true;
}

// (macroexpand form) expands form while it's a macro call (but not the forms
// inside it), to show what a macro makes
bool builtin_macroexpand(Snap* snap, const SValue* args, int num_args, SValue* result) {
  int num_anchors = snap->anchors.vsize;
  SValue form, macro;
  if (num_args != 1) return arity_error(snap, result);
  form = args[0];
  while (is_cons(form) && !is_empty(form) && is_sym(as_cons(form)->first)) {
    macro = as_sym(as_cons(form)->first)->value;
    if (!is_obj(macro) || !as_obj(macro)->macro) break;
    form = macro_expand(snap, macro, as_cons(as_cons(form)->rest));
    snap->anchors.vsize = num_anchors; // Releases the last expansion
    if (is_err(form)) {
      *result = form;
      return false;
    }
    if (is_obj(form)) anchor(snap, as_obj(form));
  }
  snap->anchors.vsize = num_anchors;
  *result = form;
  return true;
}

// (apply f a b xs) calls f with a, b and the values in xs. The VM does this
// itself (see CALL), so this is for C calling it with snap_call(), and for
// the errors. A function that returns an error value looks like it failed,
// as snap_call() can't tell them apart.
bool builtin_apply(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SValue seq, *argv;
  int len, n, i;
  if (num_args < 2) return arity_error(snap, result);
  seq = args[num_args - 1];
  if (!is_seq(seq)) return fail(snap, result, "snap_type_error", "Expected a vector or list as apply's last argument");
  len = seq_length(seq);
  n = num_args - 2 + len;
  argv = (SValue*)malloc((n + 1) * sizeof(SValue));
  for (i = 0; i < num_args - 2; ++i) argv[i] = args[i + 1];
  if (is_vec(seq)) {
    for (i = 0; i < len; ++i) argv[num_args - 2 + i] = *snap_vec_at(as_vec(seq), i);
  } else {
    for (i = num_args - 2; !is_nil(seq) && !is_empty(seq); seq = as_cons(seq)->rest) argv[i++] = as_cons(seq)->first;
  }
  *result = snap_call(snap, args[0], argv, n); // The values are rooted by args and the call
  free(argv);
  return !is_err(*result);
}

/* Operators as values */

// What the arithmetic instructions do, folding left over two or more numbers
static bool arith(Snap* snap, int op, const SValue* args, int num_args, SValue* result) {
  SValue acc;
  int i;
  if (num_args < 2 && !((op == TK_SUB || op == TK_ADD) && num_args == 1)) return arity_error(snap, result);
  acc = num_args == 1 ? create_int(0) : args[0]; // (- x) is (- 0 x), (+ x) is (+ 0 x)
  for (i = num_args == 1 ? 0 : 1; i < num_args; ++i) {
    const SValue* a = &acc, * b = &args[i];
    if (!is_num_p(a) || !is_num_p(b)) return fail(snap, result, "snap_type_error", "Expected numbers");
    if ((op == TK_DIV || op == TK_MOD) && int_zero_divisor()) {
      return fail(snap, result, "snap_arith_error", "Division by zero");
    }
    if (is_int_p(a) && is_int_p(b)) {
      switch (op) {
        case TK_ADD: acc.bits = add_iop(a->bits, b->bits); break;
        case TK_SUB: acc.bits = sub_iop(a->bits, b->bits); break;
        case TK_MUL: acc.bits = mul_iop(a->bits, b->bits); break;
        case TK_DIV: acc.bits = div_iop(a->bits, b->bits); break;
        default: acc.bits = mod_iop(a->bits, b->bits); break;
      }
    } else {
      SnapFloat x = num_p(a), y = num_p(b), f;
      switch (op) {
        case TK_ADD: f = add_fop(x, y); break;
        case TK_SUB: f = sub_fop(x, y); break;
        case TK_MUL: f = mul_fop(x, y); break;
        case TK_DIV: f = div_fop(x, y); break;
        default: f = mod_fop(x, y); break;
      }
      acc = float_new(snap, f); // Only used by the next step, before anything else allocates
    }
  }
  *result = acc;
  return true;
}

#define compare_vals(op) \
  (is_int_p(a) && is_int_p(b) ? (SnapInt)a->bits op (SnapInt)b->bits \
   : is_num_p(a) && is_num_p(b) ? num_p(a) op num_p(b) : snap_compare(a, b) op 0)

// What the compare instructions do: true if each pair of values in a row is
static bool compare_args(Snap* snap, int op, const SValue* args, int num_args, SValue* result) {
  bool cond = true;
  int i;
  if (num_args < 2) return arity_error(snap, result);
  for (i = 1; i < num_args && cond; ++i) {
    const SValue* a = &args[i - 1], * b = &args[i];
    switch (op) {
      case TK_LT: cond = compare_vals(<); break;
      case TK_LE: cond = compare_vals(<=); break;
      case TK_GT: cond = compare_vals(>); break;
      case TK_GE: cond = compare_vals(>=); break;
      default: cond = compare_vals(==); break;
    }
  }
  *result = create_bool(cond);
  return true;
}

bool builtin_not(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_bool(is_falsy(args[0]));
  return true;
}

#define XX(name, fn, token) \
  static bool name(Snap* snap, const SValue* args, int num_args, SValue* result) { \
    return fn(snap, token, args, num_args, result); \
  }
XX(builtin_add, arith, TK_ADD) XX(builtin_sub, arith, TK_SUB) XX(builtin_mul, arith, TK_MUL)
XX(builtin_div, arith, TK_DIV) XX(builtin_mod, arith, TK_MOD)
XX(builtin_lt, compare_args, TK_LT) XX(builtin_le, compare_args, TK_LE)
XX(builtin_gt, compare_args, TK_GT) XX(builtin_ge, compare_args, TK_GE)
XX(builtin_eq, compare_args, TK_EQ)
#undef XX

static bool builtin_ne(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (!compare_args(snap, TK_EQ, args, num_args, result)) return false;
  *result = create_bool(is_falsy(*result));
  return true;
}

// Sets *val to the C function for an operator. Returns false if the form
// isn't one.
bool op_value(int token, SValue* val) {
  static const SCFunc ops[] = {
    builtin_add, builtin_sub, builtin_mul, builtin_div, builtin_mod,
    builtin_lt, builtin_le, builtin_gt, builtin_ge, builtin_eq
  };
  if (token == TK_NOT) {
    *val = create_cfunc(builtin_not);
  } else if (token == TK_NE) {
    *val = create_cfunc(builtin_ne);
  } else if (token >= TK_ADD && token <= TK_EQ) {
    *val = create_cfunc(ops[token - TK_ADD]);
  } else {
    return false;
  }
  return true;
}

/* Math */

// (sqrt x), (sin x), (cos x) and (atan2 y x) of ints or floats are floats
#define XX(name, expr) \
  static bool name(Snap* snap, const SValue* args, int num_args, SValue* result) { \
    if (num_args != 1 || !is_num_p(&args[0])) return fail(snap, result, "snap_type_error", "Expected a number"); \
    SnapFloat x = num_p(&args[0]); \
    *result = expr; \
    return true; \
  }
XX(builtin_sqrt, float_new(snap, sqrt(x))) XX(builtin_sin, float_new(snap, sin(x)))
XX(builtin_cos, float_new(snap, cos(x))) XX(builtin_float, float_new(snap, x))
XX(builtin_abs, is_int(args[0]) ? create_int(llabs(as_int(args[0]))) : float_new(snap, fabs(x)))
XX(builtin_tan, float_new(snap, tan(x))) XX(builtin_asin, float_new(snap, asin(x)))
XX(builtin_acos, float_new(snap, acos(x))) XX(builtin_exp, float_new(snap, exp(x)))
XX(builtin_log, float_new(snap, log(x)))
#undef XX

// (floor x), (ceil x), (round x) (half away from zero) and (int x) (toward
// zero) are ints. Floats too big for one (or NaN) are an error.
static bool to_int(Snap* snap, const SValue* args, int num_args, double (*round_fn)(double), SValue* result) {
  SnapFloat x;
  if (num_args != 1 || !num_val(&args[0], &x)) return fail(snap, result, "snap_type_error", "Expected a number");
  if (is_int(args[0])) {
    *result = args[0];
    return true;
  }
  if (round_fn) x = round_fn(x);
  if (!in_int_range(x)) return fail(snap, result, "snap_arith_error", "Number is too big for an integer");
  *result = create_int((SnapInt)x);
  return true;
}

static bool builtin_floor(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return to_int(snap, args, num_args, floor, result);
}

static bool builtin_ceil(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return to_int(snap, args, num_args, ceil, result);
}

static bool builtin_round(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return to_int(snap, args, num_args, round, result);
}

// (mod a b) is the remainder with b's sign, so (mod -1 4) is 3. % keeps a's.
static bool builtin_mod_floor(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 2) return arity_error(snap, result);
  if (!is_num_p(&args[0]) || !is_num_p(&args[1])) return fail(snap, result, "snap_type_error", "Expected numbers");
  if (is_int(args[0]) && is_int(args[1])) {
    SnapInt a = as_int(args[0]), b = as_int(args[1]), r;
    if (b == 0) return fail(snap, result, "snap_arith_error", "Division by zero");
    r = a % b;
    if (r != 0 && (r < 0) != (b < 0)) r += b;
    *result = create_int(r);
  } else {
    SnapFloat a = num_p(&args[0]), b = num_p(&args[1]), r = fmod(a, b);
    if (r != 0 && (r < 0) != (b < 0)) r += b;
    *result = float_new(snap, r);
  }
  return true;
}

bool builtin_int(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return to_int(snap, args, num_args, NULL, result);
}

// (pow x y) is x to the power y, a float
static bool builtin_pow(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 2 || !is_num_p(&args[0]) || !is_num_p(&args[1])) {
    return fail(snap, result, "snap_type_error", "Expected (pow <x> <y>)");
  }
  *result = float_new(snap, pow(num_p(&args[0]), num_p(&args[1])));
  return true;
}

// (bit-and a b), (bit-or a b), (bit-xor a b), (bit-shl a n) and (bit-shr a n)
// (which keeps the sign) of ints. Shifts are by 0 to 63, and what's shifted
// out of an int's 63 bits is lost.
#define XX(name, shift, expr) \
  static bool name(Snap* snap, const SValue* args, int num_args, SValue* result) { \
    SnapInt a, b; \
    if (num_args != 2 || !is_int(args[0]) || !is_int(args[1])) return fail(snap, result, "snap_type_error", "Expected two integers"); \
    a = as_int(args[0]); \
    b = as_int(args[1]); \
    if (shift && (b < 0 || b > 63)) { \
      return fail(snap, result, "snap_arith_error", "Shift must be 0 to 63"); \
    } \
    *result = expr; \
    return true; \
  }
XX(builtin_bit_and, false, create_int(a & b)) XX(builtin_bit_or, false, create_int(a | b))
XX(builtin_bit_xor, false, create_int(a ^ b))
XX(builtin_bit_shl, true, create_int((SnapInt)((uint64_t)a << b)))
XX(builtin_bit_shr, true, create_int(a >> b))
#undef XX

bool builtin_atan2(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 2 || !is_num_p(&args[0]) || !is_num_p(&args[1])) {
    return fail(snap, result, "snap_type_error", "Expected (atan2 <y> <x>)");
  }
  *result = float_new(snap, atan2(num_p(&args[0]), num_p(&args[1])));
  return true;
}

// (min x y...) and (max x y...) are the smallest and largest of their
// arguments
static bool min_max(Snap* snap, int op, const SValue* args, int num_args, SValue* result) {
  int i, best = 0;
  if (num_args < 1) return arity_error(snap, result);
  for (i = 0; i < num_args; ++i) {
    if (!is_num_p(&args[i])) return fail(snap, result, "snap_type_error", "Expected numbers");
    if (op == TK_LT ? num_p(&args[i]) < num_p(&args[best]) : num_p(&args[i]) > num_p(&args[best])) best = i;
  }
  *result = args[best];
  return true;
}

bool builtin_min(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return min_max(snap, TK_LT, args, num_args, result);
}

bool builtin_max(Snap* snap, const SValue* args, int num_args, SValue* result) {
  return min_max(snap, TK_GT, args, num_args, result);
}

// (rand) is a float from 0 up to 1, and (rand n) an int from 0 up to n
// (xorshift64*)
bool builtin_rand(Snap* snap, const SValue* args, int num_args, SValue* result) {
  uint64_t r, state = snap->rand_state;
  if (num_args > 1 || (num_args == 1 && (!is_int(args[0]) || as_int(args[0]) <= 0))) {
    return fail(snap, result, "snap_type_error", "Expected (rand) or (rand <positive int>)");
  }
  state ^= state >> 12;
  state ^= state << 25;
  state ^= state >> 27;
  snap->rand_state = state;
  r = state * 0x2545f4914f6cdd1d;
  *result = num_args == 0 ? float_new(snap, (SnapFloat)(r >> 11) / (SnapFloat)(1ull << 53))
                          : create_int((SnapInt)(r % (uint64_t)as_int(args[0])));
  return true;
}

// (rand-seed n) starts (rand) over from n, so what it gives can be repeated
static bool builtin_rand_seed(Snap* snap, const SValue* args, int num_args, SValue* result) {
  uint64_t z;
  if (num_args != 1 || !is_int(args[0])) return fail(snap, result, "snap_type_error", "Expected (rand-seed <int>)");
  // splitmix64 of n, which is never 0 (xorshift's one bad state) for any int
  z = (uint64_t)as_int(args[0]) + 0x9e3779b97f4a7c15;
  z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9;
  z = (z ^ (z >> 27)) * 0x94d049bb133111eb;
  z ^= z >> 31;
  snap->rand_state = z ? z : 0x9e3779b97f4a7c15;
  *result = create_nil();
  return true;
}

// (load "file") runs a file and gives its last value, or an error (see
// snap_load())
static bool builtin_load(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1 || !is_str(args[0])) return fail(snap, result, "snap_type_error", "Expected (load <path>)");
  *result = snap_load(snap, as_str(args[0])->data);
  return true;
}

/* Constants */

// (set-const! 'name val) makes a global that can't change (see defconst in
// stdlib.snap). Its symbol is marked read-only, and code compiled after this
// uses the value in place of the global.
bool builtin_set_const(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SSym* sym;
  if (num_args != 2 || !is_sym(args[0])) return fail(snap, result, "snap_type_error", "Expected (set-const! <name> <value>)");
  sym = as_sym(args[0]);
  if (sym->read_only) {
    *result = create_obj(snap_err_new_format(snap, "snap_type_error", "%s is already a constant", sym->data));
    return false;
  }
  gc_barrier(snap, sym->value);
  sym->value = args[1];
  sym->read_only = true;
  *result = create_nil();
  return true;
}

/* Types and methods */

bool builtin_type(Snap* snap, const SValue* args, int num_args, SValue* result) {
  if (num_args != 1) return arity_error(snap, result);
  *result = create_obj(type_of(snap, args[0]));
  return true;
}

// (make-type 'Name [field...]) makes a struct type (see defstruct). The fields
// can be symbols or keywords; they're keywords in the type.
bool builtin_make_type(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SVec* fields;
  int i;
  if (num_args != 2 || !is_sym(args[0]) || !is_vec(args[1])) {
    return fail(snap, result, "snap_type_error", "Expected (make-type <name> [<field>...])");
  }
  fields = (SVec*)anchor(snap, (SObject*)snap_vec_new(snap, as_vec(args[1])->len));
  for (i = 0; i < as_vec(args[1])->len; ++i) {
    SValue f = *snap_vec_at(as_vec(args[1]), i);
    if (is_sym(f)) f = create_obj(snap_key_new(snap, as_sym(f)->data));
    if (!is_key(f)) {
      release(snap);
      return fail(snap, result, "snap_type_error", "Expected fields to be symbols or keywords");
    }
    vec_push(snap, fields, f);
  }
  *result = create_obj(type_new(snap, as_sym(args[0])->data, fields));
  release(snap);
  return true;
}

// (add-method! 'name Type fn) makes fn the implementation of method name for
// Type, defining the method if it isn't yet (see defmethod)
bool builtin_add_method(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SSym* name;
  SMethod* m;
  if (num_args != 3 || !is_sym(args[0]) || !is_type(args[1])) {
    return fail(snap, result, "snap_type_error", "Expected (add-method! <name> <type> <fn>)");
  }
  name = as_sym(args[0]);
  if (is_method(name->value)) {
    m = as_method(name->value);
  } else if (is_undef(name->value)) {
    SMap* table = (SMap*)anchor(snap, (SObject*)snap_map_new(snap));
    m = (SMethod*)gc_new(snap, STYPE_METHOD, sizeof(SMethod));
    m->name = name;
    m->table = table;
    name->value = create_obj(m);
    release(snap);
  } else {
    *result = create_obj(snap_err_new_format(snap, "snap_type_error",
                                             "%s is already defined and isn't a method", name->data));
    return false;
  }
  snap_map_put(snap, m->table, args[1], args[2]);
  *result = create_obj(m);
  return true;
}

// (set-gc! Type f) has f called with each struct of Type made from now on,
// once nothing can reach it (see defgc). (set-gc! Type nil) takes it away.
bool builtin_set_gc(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SValue fn = num_args == 2 ? args[1] : create_nil();
  if (num_args != 2 || !is_type(args[0]) || !as_type(args[0])->is_struct ||
      !(is_nil(fn) || is_cfunc(fn) || is_code(fn) || is_closure(fn) || is_method(fn))) {
    return fail(snap, result, "snap_type_error", "Expected (set-gc! <struct type> <fn or nil>)");
  }
  snap_set_gc(snap, as_type(args[0]), fn);
  *result = args[0];
  return true;
}

// (find-method m Type) is the function method m (or the method named by the
// symbol m) runs for Type, or nil if there isn't one
bool builtin_find_method(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SValue m, * fn = NULL;
  if (num_args != 2 || !is_type(args[1])) {
    return fail(snap, result, "snap_type_error", "Expected (find-method <method> <type>)");
  }
  m = is_sym(args[0]) ? as_sym(args[0])->value : args[0];
  if (is_method(m)) fn = map_get(as_method(m)->table, args[1]);
  *result = fn ? *fn : create_nil();
  return true;
}

// Defines the builtins as globals (snap_init() does this first)
void snap_init_builtins(Snap* snap) {
  snap_def_cfunc(snap, "print", builtin_print);
  snap_def_cfunc(snap, "println", builtin_println);
  snap_def_cfunc(snap, "nil?", builtin_isnil);
  snap_def_cfunc(snap, "vector?", builtin_isvec);
  snap_def_cfunc(snap, "int?", builtin_isint);
  snap_def_cfunc(snap, "float?", builtin_isfloat);
  snap_def_cfunc(snap, "number?", builtin_isnumber);
  snap_def_cfunc(snap, "str?", builtin_isstr);
  snap_def_cfunc(snap, "key?", builtin_iskey);
  snap_def_cfunc(snap, "bool?", builtin_isbool);
  snap_def_cfunc(snap, "fn?", builtin_isfn);
  snap_def_cfunc(snap, "struct?", builtin_isstruct);
  snap_def_cfunc(snap, "type?", builtin_istype);
  snap_def_cfunc(snap, "map?", builtin_ismap);
  snap_def_cfunc(snap, "err", builtin_err);
  snap_def_cfunc(snap, "err?", builtin_iserr);
  snap_def_cfunc(snap, "err-kind", builtin_err_kind);
  snap_def_cfunc(snap, "err-msg", builtin_err_msg);
  snap_def_cfunc(snap, "err-where", builtin_err_where);
  snap_def_cfunc(snap, "err-trace", builtin_err_trace);
  snap_def_cfunc(snap, "count", builtin_count);
  snap_def_cfunc(snap, "nth", builtin_nth);
  snap_def_cfunc(snap, "apply", builtin_apply);
  snap_def_cfunc(snap, "range", builtin_range);
  snap_def_cfunc(snap, "vector", builtin_vector);
  snap_def_cfunc(snap, "assoc", builtin_assoc);
  snap_def_cfunc(snap, "dissoc", builtin_dissoc);
  snap_def_cfunc(snap, "merge", builtin_merge);
  snap_def_cfunc(snap, "put!", builtin_put);
  snap_def_cfunc(snap, "push!", builtin_push);
  snap_def_cfunc(snap, "pop!", builtin_pop);
  snap_def_cfunc(snap, "get", builtin_get);
  snap_def_cfunc(snap, "del!", builtin_del);
  snap_def_cfunc(snap, "keys", builtin_keys);
  snap_def_cfunc(snap, "vals", builtin_vals);
  snap_def_cfunc(snap, "str", builtin_str);
  snap_def_cfunc(snap, "name", builtin_name);
  snap_def_cfunc(snap, "format", builtin_format);
  snap_def_cfunc(snap, "compile", builtin_compile);
  snap_def_cfunc(snap, "gc-step", builtin_gc_step);
  snap_def_cfunc(snap, "gc-stats", builtin_gc_stats);
  snap_def_cfunc(snap, "clock", builtin_clock);
  snap_def_cfunc(snap, "cpu-time", builtin_cpu_time);
  snap_def_cfunc(snap, "alloc-profile!", builtin_alloc_profile_set);
  snap_def_cfunc(snap, "alloc-profile", builtin_alloc_profile);
  snap_def_cfunc(snap, "set-const!", builtin_set_const);
  snap_def_cfunc(snap, "list", builtin_list);
  snap_def_cfunc(snap, "cons", builtin_cons);
  snap_def_cfunc(snap, "first", builtin_first);
  snap_def_cfunc(snap, "rest", builtin_rest);
  snap_def_cfunc(snap, "next", builtin_next);
  snap_def_cfunc(snap, "nthnext", builtin_nthnext);
  snap_def_cfunc(snap, "concat", builtin_concat);
  snap_def_cfunc(snap, "vec", builtin_vec);
  snap_def_cfunc(snap, "list?", builtin_islist);
  snap_def_cfunc(snap, "sym?", builtin_issym);
  snap_def_cfunc(snap, "gensym", builtin_gensym);
  snap_def_cfunc(snap, "macroexpand", builtin_macroexpand);
  snap_def_cfunc(snap, "type", builtin_type);
  snap_def_cfunc(snap, "make-type", builtin_make_type);
  snap_def_cfunc(snap, "add-method!", builtin_add_method);
  snap_def_cfunc(snap, "find-method", builtin_find_method);
  snap_def_cfunc(snap, "set-gc!", builtin_set_gc);
  snap_def_cfunc(snap, "sqrt", builtin_sqrt);
  snap_def_cfunc(snap, "sin", builtin_sin);
  snap_def_cfunc(snap, "cos", builtin_cos);
  snap_def_cfunc(snap, "atan2", builtin_atan2);
  snap_def_cfunc(snap, "floor", builtin_floor);
  snap_def_cfunc(snap, "mod", builtin_mod_floor);
  snap_def_cfunc(snap, "int", builtin_int);
  snap_def_cfunc(snap, "float", builtin_float);
  snap_def_cfunc(snap, "abs", builtin_abs);
  snap_def_cfunc(snap, "min", builtin_min);
  snap_def_cfunc(snap, "max", builtin_max);
  snap_def_cfunc(snap, "rand", builtin_rand);
  snap_def_cfunc(snap, "rand-seed", builtin_rand_seed);
  snap_def_cfunc(snap, "tan", builtin_tan);
  snap_def_cfunc(snap, "asin", builtin_asin);
  snap_def_cfunc(snap, "acos", builtin_acos);
  snap_def_cfunc(snap, "exp", builtin_exp);
  snap_def_cfunc(snap, "log", builtin_log);
  snap_def_cfunc(snap, "pow", builtin_pow);
  snap_def_cfunc(snap, "ceil", builtin_ceil);
  snap_def_cfunc(snap, "round", builtin_round);
  snap_def_cfunc(snap, "bit-and", builtin_bit_and);
  snap_def_cfunc(snap, "bit-or", builtin_bit_or);
  snap_def_cfunc(snap, "bit-xor", builtin_bit_xor);
  snap_def_cfunc(snap, "bit-shl", builtin_bit_shl);
  snap_def_cfunc(snap, "bit-shr", builtin_bit_shr);
  snap_def_cfunc(snap, "load", builtin_load);
}
