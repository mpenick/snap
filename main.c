#include "snap.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
  int i;
  Snap snap;
  snap_init(&snap);

  if (argc < 2) {
    fprintf(stderr, "%s <files>\n", argv[0]);
    exit(-1);
  }

  for (i = 1; i < argc; ++i) {
    FILE* file = fopen(argv[i], "r");
    if (file) {
      long int end;
      size_t num_bytes;
      char* buf;
      fseek(file, 0, SEEK_END);
      end = ftell(file);
      fseek(file, 0, SEEK_SET);
      buf = malloc(end + 1);
      if ((num_bytes = fread(buf, 1, end, file)) > 0) {
        buf[num_bytes] = '\0';
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

  snap_destroy(&snap);

  return 0;
}
