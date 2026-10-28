#include "snap.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
  const char* filename;
  int rc = 0;
  Snap snap;
  snap_init(&snap);

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
    buf = (char*)malloc(end + 1);
    if ((num_bytes = fread(buf, 1, end, file)) > 0) {
      int i;
      SValue args = create_empty(), result;
      SCons* tail = NULL;
      for (i = 2; i < argc; ++i) {
        SCons* next = snap_cons_new(&snap);
        if (tail) {
          tail->rest = create_obj(next);
        } else {
          args = create_obj(snap_anchor(&snap, (SObject*)next));
        }
        tail = next;
        tail->first = create_obj(snap_str_new(&snap, argv[i]));
      }
      snap_def(&snap, "args", args);
      if (tail) snap_release(&snap);
      buf[num_bytes] = '\0';
      result = snap_exec(&snap, buf);
      if (is_err(result)) {
        snap_print(result);
        printf("\n");
        rc = -1;
      }
    } else {
      fprintf(stderr, "Unable to read %s\n", filename);
    }
    fclose(file);
    free(buf);
  } else {
    fprintf(stderr, "'%s' not found\n", argv[1]);
  }

  snap_destroy(&snap);

  return rc;
}
