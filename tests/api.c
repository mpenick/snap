// Tests of the C API that scripts can't reach: pins, under a collector that
// only runs when told to. `make stress` runs it under ASan, which catches a
// pinned value that gets freed.
#include "snap.h"

#include <stdio.h>
#include <string.h>

static int failed = 0;

#define CHECK(cond)                                         \
  do {                                                      \
    if (!(cond)) {                                          \
      printf("FAIL: %s:%d: %s\n", __FILE__, __LINE__, #cond); \
      failed++;                                             \
    }                                                       \
  } while (0)

static int num_finalized;
static SnapInt sum_finalized;

// A C finalizer for (defstruct Handle [id]), like a binding freeing what a
// handle names
static bool handle_gc(SnapArgs* a) {
  num_finalized++;
  sum_finalized += as_int(as_struct(a->args[0])->vals[0]);
  return true;
}

// Runs two whole collections, so that one starts after everything above
static void collect(Snap* snap) {
  int i;
  for (i = 0; i < 2; ++i) {
    snap_gc_step(snap, 0);
    while (!snap_gc_step(snap, 1000)) {}
  }
}

// Reads a road, {:width w :pts [[x z] ...]} or a Road struct, and an optional
// scale; returns the width times the scale plus the sum of the points, or the
// read's error
static bool road_sum(SnapArgs* a) {
  SValue road = snap_arg(a, 0);
  double sum = snap_num(a, snap_get(a, road, "width")), scale;
  SVec* pts = snap_vec(a, snap_get(a, road, "pts"));
  float xz[2];
  int i;
  for (i = 0; i < pts->len; ++i) {
    int n = snap_nums(a, snap_at(a, pts, i), xz, 2);
    if (n == 2) sum += xz[0] + xz[1];
    else if (!a->failed) snap_args_fail(a, "should have 2 numbers");
  }
  scale = snap_num_or(a, snap_arg(a, 1), 1.0);
  if (snap_truthy(a, snap_get(a, road, "double"))) sum *= 2.0;
  if (a->num_args > 2) snap_name(a, snap_arg(a, 2));
  if (a->failed) return false;
  *a->result = create_float(a->snap, sum * scale);
  return true;
}

// Sums its numbers, checking the name
static bool spaced(SnapArgs* a) {
  SnapInt sum = snap_int(a, snap_arg(a, 0));
  int i;
  snap_name(a, snap_arg(a, 1));
  for (i = 2; i < a->num_args; ++i) sum += snap_int(a, snap_arg(a, i));
  if (a->failed) return false;
  *a->result = create_int(sum);
  return true;
}

// Handles for slots, as a binding keeps them: (slot-scale slot x y z) is
// slot's scale times the sum of x, y and z, if slot is a live one
static double slot_scales[4] = { 1, 2, 0, 0 };

static double* get_slot(SValue v) {
  return is_int(v) && as_int(v) >= 0 && as_int(v) < 4 && slot_scales[as_int(v)] != 0 ? &slot_scales[as_int(v)] : NULL;
}

static bool slot_scale(SnapArgs* a) {
  double* scale = snap_ptr(a, get_slot(snap_arg(a, 0)), "a live slot");
  double xyz[3];
  snap_arg_nums(a, 1, 3, xyz);
  if (scale && *scale < 0) snap_args_fail_at(a, 0, "is scaled backward");
  if (a->failed) return false;
  *a->result = create_float(a->snap, *scale * (xyz[0] + xyz[1] + xyz[2]));
  return true;
}

// Gives back the empty vector a failed read gives, which is read-only
static bool bad_vec(SnapArgs* a) {
  *a->result = create_obj(snap_vec(a, snap_arg(a, 0)));
  return true;
}

// The message of err, a string
static const char* err_msg(SValue err) {
  return is_err(err) && is_str(as_err(err)->msg) ? as_str(as_err(err)->msg)->data : "";
}

int main(void) {
  Snap snap;
  SValue a, b, c, fn, r;
  snap_init(&snap);
  snap.gc_manual = true;

  // Pinned values survive with nothing else holding them, and pinning twice
  // takes two unpins
  a = snap_pin(&snap, create_obj(snap_str_new(&snap, "a")));
  b = snap_pin(&snap, create_obj(snap_str_new(&snap, "b")));
  snap_pin(&snap, a);
  collect(&snap);
  CHECK(strcmp(as_str(a)->data, "a") == 0 && strcmp(as_str(b)->data, "b") == 0);
  snap_unpin(&snap, a);
  collect(&snap);
  CHECK(strcmp(as_str(a)->data, "a") == 0);

  // In any order: unpinning a leaves b and c
  c = snap_pin(&snap, create_obj(snap_str_new(&snap, "c")));
  snap_unpin(&snap, a);
  collect(&snap);
  CHECK(strcmp(as_str(b)->data, "b") == 0 && strcmp(as_str(c)->data, "c") == 0);
  CHECK(snap.pins.vsize == 2);

  // Pins outlast a panic, which unwinds the anchors
  fn = snap_pin(&snap, snap_exec(&snap, "(let [v [1 2 3]] (fn [] (count v)))"));
  r = snap_exec(&snap, "(panic :oops \"unwound\")");
  CHECK(is_err(r));
  collect(&snap);
  r = snap_call(&snap, fn, NULL, 0);
  CHECK(is_int(r) && as_int(r) == 3);

  // apply called from C goes through snap_call() itself
  {
    SValue args[3];
    args[0] = snap_exec(&snap, "+");
    args[1] = create_int(1);
    args[2] = snap_pin(&snap, snap_exec(&snap, "[2 3]"));
    r = snap_call(&snap, snap_exec(&snap, "apply"), args, 3);
    CHECK(is_int(r) && as_int(r) == 6);
    r = snap_call(&snap, snap_exec(&snap, "apply"), args, 2);
    CHECK(is_err(r));
    snap_unpin(&snap, args[2]);
  }

  // Values that aren't objects are fine to pin and unpin
  snap_pin(&snap, create_int(1));
  snap_unpin(&snap, create_int(1));
  snap_unpin(&snap, b);
  snap_unpin(&snap, c);
  snap_unpin(&snap, fn);
  CHECK(snap.pins.vsize == 0);

  snap_destroy(&snap);

  // Finalizers: a C one, and snap ones that run in the middle of a function
  // (after a C call returns) and move the stack by recursing, which the VM
  // has to reload its pointers after. The collector runs by itself here.
  snap_init(&snap);
  r = snap_exec(&snap, "(defstruct Handle [id]) Handle");
  CHECK(is_type(r));
  snap_set_gc(&snap, as_type(r), snap_cfunc(&snap, "(handle-gc h)", handle_gc));
  r = snap_exec(&snap,
    "(defn deep [n] (if (= n 0) 0 (+ 1 (deep (- n 1)))))\n"
    "(defstruct Deep [n])\n"
    "(def depth 0)\n"
    "(defgc [d Deep] (set! depth (max depth (deep 3000))))\n"
    "(defn churn [n]\n"
    "  (loop [i 0 acc 0]\n"
    "    (if (< i n)\n"
    "      (begin (Deep i) (Handle i) (gc-step 200) (recur (+ i 1) (+ acc (abs i))))\n"
    "      acc)))\n"
    "(churn 2000)");
  CHECK(is_int(r) && as_int(r) == 1999 * 2000 / 2);
  r = snap_exec(&snap, "depth");
  CHECK(is_int(r) && as_int(r) == 3000);
  CHECK(num_finalized > 0);

  // snap_destroy() finalizes the rest, live or not, each exactly once
  r = snap_exec(&snap, "(def live (Handle 1000000))");
  snap_destroy(&snap);
  CHECK(num_finalized == 2001);
  CHECK(sum_finalized == 1999 * 2000 / 2 + 1000000);

  // Reading arguments from C (SnapArgs)
  snap_init(&snap);
  snap_def_cfunc(&snap, "(road-sum road [scale] [label])", road_sum);
  r = snap_exec(&snap, "(road-sum {:width 2 :pts [[1 2] [3.5 4]]})");
  CHECK(is_float(r) && as_float(r) == 12.5);
  r = snap_exec(&snap, "(road-sum {:width 2 :pts [] :double true} 0.5 :label)");
  CHECK(is_float(r) && as_float(r) == 2.0);
  r = snap_exec(&snap, "(defstruct Road [pts width]) (road-sum (Road [[1 1]] 1) 2)");
  CHECK(is_float(r) && as_float(r) == 6.0);
  r = snap_exec(&snap, "(road-sum)");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): got 0 arguments") == 0);
  r = snap_exec(&snap, "(road-sum {} 1 2 3)");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): got 4 arguments") == 0);
  r = snap_exec(&snap, "(road-sum {:pts []})");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): road, :width is missing (it should be a number)") == 0);
  r = snap_exec(&snap, "(road-sum {:width nil :pts []})");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): road, :width should be a number, not nil") == 0);
  r = snap_exec(&snap, "(road-sum [1 2])");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): road should be a map or struct, not a vector") == 0);
  r = snap_exec(&snap, "(road-sum {:width 1 :pts [[1 2] [3 :x]]})");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): road, :pts, item 1 should be numbers (item 1 isn't), not a vector") == 0);
  r = snap_exec(&snap, "(road-sum {:width 1 :pts [[1 2 3]]})");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): road, :pts, item 0 should be a vector of at most 2 numbers, not a vector") == 0);
  r = snap_exec(&snap, "(road-sum {:width 1 :pts [[1]]})");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): road, :pts, item 0 should have 2 numbers") == 0);
  r = snap_exec(&snap, "(road-sum {:width 1 :pts []} \"big\")");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): scale should be a number, not a str") == 0);
  r = snap_exec(&snap, "(road-sum {:width 1 :pts []} 1 5)");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): label should be a string, keyword or symbol, not an int") == 0);
  r = snap_exec(&snap, "(road-sum {:width 1 :pts [[1 2] [3 4] [5]]} 1)");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): road, :pts, item 2 should have 2 numbers") == 0);
  r = snap_exec(&snap, "(road-sum (Road [] 1) 1 (Road [] 2))");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): label should be a string, keyword or symbol, not a Road") == 0);
  r = snap_exec(&snap, "(road-sum nil)");
  CHECK(strcmp(err_msg(r), "(road-sum road [scale] [label]): road, :width is missing (it should be a number)") == 0);
  // <name> can hold spaces; name... takes the rest
  snap_def_cfunc(&snap, "(spaced <first number> <a name> <more...>)", spaced);
  r = snap_exec(&snap, "(spaced 1 :x 2 3)");
  CHECK(is_int(r) && as_int(r) == 6);
  r = snap_exec(&snap, "(spaced 1 :x)");
  CHECK(is_int(r) && as_int(r) == 1);
  r = snap_exec(&snap, "(spaced 1)");
  CHECK(strcmp(err_msg(r), "(spaced <first number> <a name> <more...>): got 1 argument") == 0);
  r = snap_exec(&snap, "(spaced 1 2)");
  CHECK(strcmp(err_msg(r), "(spaced <first number> <a name> <more...>): a name should be a string, keyword or symbol, not an int") == 0);
  r = snap_exec(&snap, "(spaced 1 :x 2 :y)");
  CHECK(strcmp(err_msg(r), "(spaced <first number> <a name> <more...>): more should be an int, not a key") == 0);
  snap_destroy(&snap);

  // A table of them; handles by the binding's lookup (snap_ptr), numbers by
  // the run (snap_arg_nums), and failing about an argument (snap_args_fail_at)
  {
    // The usages are copied, so they needn't outlast the definitions
    char usage[64];
    SnapCFuncDef defs[2];
    snprintf(usage, sizeof(usage), "(slot-scale <slot> x y z)");
    defs[0].usage = usage;
    defs[0].fn = slot_scale;
    defs[1].usage = "(bad-vec x)";
    defs[1].fn = bad_vec;
    snap_init(&snap);
    snap_def_cfuncs(&snap, defs, 2);
    memset(usage, 0, sizeof(usage));
  }
  r = snap_exec(&snap, "(slot-scale 1 1 2 3.5)");
  CHECK(is_float(r) && as_float(r) == 13.0);
  r = snap_exec(&snap, "(slot-scale 2 1 2 3)");
  CHECK(strcmp(err_msg(r), "(slot-scale <slot> x y z): slot should be a live slot, not an int") == 0);
  r = snap_exec(&snap, "(slot-scale 0 1 :y 3)");
  CHECK(strcmp(err_msg(r), "(slot-scale <slot> x y z): y should be a number, not a key") == 0);
  slot_scales[0] = -1;
  r = snap_exec(&snap, "(slot-scale 0 1 2 3)");
  CHECK(strcmp(err_msg(r), "(slot-scale <slot> x y z): slot is scaled backward") == 0);
  r = snap_exec(&snap, "(slot-scale 0 1 2)");
  CHECK(strcmp(err_msg(r), "(slot-scale <slot> x y z): got 3 arguments") == 0);
  r = snap_exec(&snap, "(push! (bad-vec 1) 2)");
  CHECK(is_err(r));
  r = snap_exec(&snap, "(count (bad-vec 1))");
  CHECK(is_int(r) && as_int(r) == 0);
  snap_destroy(&snap);

  printf(failed ? "api: %d failed\n" : "api: passed\n", failed);
  return failed != 0;
}
