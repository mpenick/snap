#include "snap.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
  const char* filename;
  Snap snap;
  snap_init(&snap);

#if 0
  if (argc < 2) {
    fprintf(stderr, "%s <file> <arg_1>...<arg_n>\n", argv[0]);
    exit(-1);
  }

  filename = argv[1];
  FILE* file = fopen(filename, "r");
  if (file) {
    long int end;
    size_t num_bytes;
    char* buf;
    fseek(file, 0, SEEK_END);
    end = ftell(file);
    fseek(file, 0, SEEK_SET);
    buf = malloc(end + 1);
    if ((num_bytes = fread(buf, 1, end, file)) > 0) {
      int i;
      SValue val;
      SCons* first = NULL;
      SCons** args = &first;
      for (i = 2; i < argc; ++i) {
        *args = first ? snap_cons_new(&snap)
          : (SCons*)snap_push(&snap, (SObject*)snap_cons_new(&snap));
        val.type = STYPE_STR;
        val.o = (SObject*)snap_str_new(&snap, argv[i]);
        (*args)->first = val;
        args = (SCons**)&(*args)->rest.o;
      }
      val.type = STYPE_CONS;
      val.o = (SObject*)first;
      snap_def(&snap, "args", val);
      if (first) snap_pop(&snap);
      buf[num_bytes] = '\0';
      snap_exec(&snap, buf);
    } else {
      fprintf(stderr, "Unable to read %s\n", filename);
    }
    fclose(file);
    free(buf);
  } else {
    fprintf(stderr, "'%s' not found\n", argv[1]);
  }
#endif

  //"(define a 1)\n"
  // "(define p (fn (x) (print x)))"
  // "(define add (fn (x y) (+ x y)))"
  // "(p (add (add (add 1 2) 3) 4))\n"

  snap_print(
        snap_exec(&snap,
                  //"(define x 1)"
                  //"(set! x 1)"
                  //"(< 1 2 3 4)"

                  //"(try "
                  //  "(println \"hello\")"
                  //  "(raise :error3 \"raised an error\")"
                  //"(catch :error1 [type val] (println type val))"
                  //"(catch :error2 [type val] (println type val))"
                  //"(catch [type val] (println type val)))"

                  //"(define func1 (fn () (raise :error2 1 2 3)))\n"
                  //"(define func2 (fn () (do (println \"here\") (func1))))\n"
                  //"(try "
                  //"(func2)"
                  //"(do (println \"success\") 0)"
                  //"(catch :error1 (fn (x) (raise x)))"
                  //"(catch :error2 (fn (x) (do (println x) 2))))"
                  //"(try "
                  //  //"(raise :error1 \"something happened\")"
                  //  "(raise \"something happened\")"
                  //  "(do (println \"success\") 0)"
                  //  "(catch :error1 (fn (x) (do (println x) 1)))"
                  //  "(catch :error2 (fn (x) (do (println x) 2))))"

                  //"(if false "
                  //    "(if true (if true (print \"true\") "
                  //              "(print \"false\"))"
                  //      "(print \"false\")) "
                  //    "(print \"false\"))\n"
                  //"(if false (define x x) (define x x))"

                  //"(define r "
                  //  "(fn [x] "
                  //  "(do (println x) "
                  //    "(if (< x 10) "
                  //      "(recur (+ x 1)) nil))))"
                  //"(r 1)"

                  //"(if false "
                  //  "(do true (if false false true)) true)"

                  "(define p (fn [x] (println x)))"
                  "(define add (fn [x y] (+ x y)))"
                  "(p (add (add (add 1 2) 3) 4))\n"
                  ));
  printf("\n");
  snap_destroy(&snap);

  return 0;
}
