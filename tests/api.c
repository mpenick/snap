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

  // Values that aren't objects are fine to pin and unpin
  snap_pin(&snap, create_int(1));
  snap_unpin(&snap, create_int(1));
  snap_unpin(&snap, b);
  snap_unpin(&snap, c);
  snap_unpin(&snap, fn);
  CHECK(snap.pins.vsize == 0);

  snap_destroy(&snap);
  printf(failed ? "api: %d failed\n" : "api: passed\n", failed);
  return failed != 0;
}
