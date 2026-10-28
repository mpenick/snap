#include "snap.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char** argv) {
  const char* filename;
  int rc = 0;
  int first = 1; // Index of the file argument
  Snap snap;
  snap_init(&snap);

  if (argc > first && strcmp(argv[first], "-d") == 0) {
    snap.dump_code = true;
    first++;
  }

  if (argc <= first) {
    fprintf(stderr, "%s [-d] <file> <arg_1>...<arg_n>\n"
                    "  -d  Print the bytecode before running\n", argv[0]);
    exit(-1);
  }

  filename = argv[first];
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
      SValue result;
      SVec* args = (SVec*)snap_anchor(&snap, (SObject*)snap_vec_new(&snap, argc - first - 1));
      for (i = first + 1; i < argc; ++i) {
        SValue arg = create_obj(snap_str_new(&snap, argv[i]));
        snap_vec_push(&snap, args, arg);
      }
      snap_def(&snap, "args", create_obj(args));
      snap_release(&snap);
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
    fprintf(stderr, "'%s' not found\n", filename);
  }

  snap_destroy(&snap);

  return rc;
}
