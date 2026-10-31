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
static bool handle_gc(Snap* snap, const SValue* args, int num_args, SValue* result) {
  num_finalized++;
  sum_finalized += as_int(as_struct(args[0])->vals[0]);
  *result = create_nil();
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
  snap_set_gc(&snap, as_type(r), create_cfunc(handle_gc));
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

  printf(failed ? "api: %d failed\n" : "api: passed\n", failed);
  return failed != 0;
}
