#include "snap.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
  Snap snap;
  snap_init(&snap);

  if (argc < 2) {
    fprintf(stderr, "%s <files>\n", argv[0]);
    exit(-1);
  }

  for (int i = 1; i < argc; ++i) {
    FILE* file = fopen(argv[i], "r");
    if (file) {
      long int end;
      void* buf;
      fseek(file, 0, SEEK_END);
      end = ftell(file);
      fseek(file, 0, SEEK_SET);
      buf = malloc(end);
      if (fread(buf, end, 1, file) > 0) {
        snap_exec(&snap, buf);
      } else {
        fprintf(stderr, "Unable to read %s\n", argv[i]);
      }
      fclose(file);
      free(buf);
    } else {
      fprintf(stderr, "'%s' not found\n", argv[i]);
    }
  }

  //snap_print(snap_exec(&snap, "((fn () (add 1 2)))"));
  //snap_print(snap_exec(&snap, "(def foo (fn () (add 1 2)))(print foo)(print (foo))"));
  //snap_print(snap_exec(&snap, "(let ((a 1) (b 2)) (add a b))"));
  //snap_print(snap_exec(&snap, "(def x 1)\n(set! x 2)(add x 1)"));
  //snap_print(snap_exec(&snap, "(do (if nil (add 1 1) (add 2 3)) (sub 2 1) (print 1))"));
  //snap_print(snap_exec(&snap, "(do (if nil (add 1 1) (add 2 3)) (sub 2 1) (print 1))"));

  //snap_print(snap_exec(&snap, "(def foo (fn (i) (do (print i) (recur (add i 1)))))(print (foo 1))"));
  //printf("\n");

  snap_destroy(&snap);

  return 0;
}
