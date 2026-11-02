// The built-in functions (snap_builtins.c), the ones other files refer to,
// and keyword calls, which are (get ...)
#ifndef SNAP_SNAP_BUILTINS_H
#define SNAP_SNAP_BUILTINS_H

#include "snap_object.h"

void snap_init_builtins(Snap* snap);
bool op_value(int token, SValue* val);
bool builtin_apply(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_concat(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_count(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_gensym(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_get(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_int(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_list(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_make_macro(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_map(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_max(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_min(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_nth(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_nthnext(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_put(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_vec(Snap* snap, const SValue* args, int num_args, SValue* result);
bool builtin_vector(Snap* snap, const SValue* args, int num_args, SValue* result);

// The C function values of the built-ins, which the compiler and VM know
// them by
#define create_builtin(f) create_cfunc(&f##_cf)
extern const SnapCFunc builtin_apply_cf;
extern const SnapCFunc builtin_concat_cf;
extern const SnapCFunc builtin_count_cf;
extern const SnapCFunc builtin_gensym_cf;
extern const SnapCFunc builtin_get_cf;
extern const SnapCFunc builtin_int_cf;
extern const SnapCFunc builtin_list_cf;
extern const SnapCFunc builtin_make_macro_cf;
extern const SnapCFunc builtin_map_cf;
extern const SnapCFunc builtin_max_cf;
extern const SnapCFunc builtin_min_cf;
extern const SnapCFunc builtin_nth_cf;
extern const SnapCFunc builtin_nthnext_cf;
extern const SnapCFunc builtin_put_cf;
extern const SnapCFunc builtin_vec_cf;
extern const SnapCFunc builtin_vector_cf;

// A keyword called as a function: (:k x) is (get x :k), (:k x d) is
// (get x :k d)
static inline bool key_call(Snap* snap, SValue key, const SValue* args, int num_args, SValue* result) {
  SValue get_args[3];
  if (num_args < 1 || num_args > 2) {
    *result = create_obj(snap_err_new_format(snap, "snap_arity_error", "Expected (:%s m) or (:%s m default)",
                                             as_key(key)->data, as_key(key)->data));
    return false;
  }
  get_args[0] = args[0];
  get_args[1] = key;
  if (num_args == 2) get_args[2] = args[1];
  return builtin_get(snap, get_args, num_args + 1, result);
}

#endif // SNAP_SNAP_BUILTINS_H
