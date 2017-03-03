#include "snap.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
  const char* filename;
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
      SValue val, result;
      SCons* first = NULL;
      SCons** args = &first;
      for (i = 2; i < argc; ++i) {
        *args = first ? snap_cons_new(&snap)
          : (SCons*)snap_anchor(&snap, (SObject*)snap_cons_new(&snap));
        val.type = STYPE_STR;
        val.o = (SObject*)snap_str_new(&snap, argv[i]);
        (*args)->first = val;
        args = (SCons**)&(*args)->rest.o;
      }
      val.type = STYPE_CONS;
      val.o = (SObject*)first;
      snap_def(&snap, "args", val);
      if (first) snap_release(&snap);
      buf[num_bytes] = '\0';
      result = snap_exec(&snap, buf);
      if (result.type == STYPE_ERR) {
        snap_print(result);
        printf("\n");
        return -1;
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

  return 0;
}
