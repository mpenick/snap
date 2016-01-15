#ifndef SNAP_HASH_H
#define SNAP_HASH_H

#include "snap_val.h"

#include <stdbool.h>

typedef struct {
  SValue key;
  SValue val;
} SEntry;

typedef struct {
  SEntry* entries;
  int capacity;
  int count;
} SnapHash;

void snap_hash_init(SnapHash* hash);
void snap_hash_destroy(SnapHash* hash);
bool snap_hash_put(SnapHash* hash, SValue key, SValue val);
bool snap_hash_delete(SnapHash* hash, SValue key);
SValue* snap_hash_get(SnapHash* hash, SValue key);
SValue* snap_hash_get_str(SnapHash* hash, const char* str, size_t len);

#endif
