#ifndef SNAP_HASH_H
#define SNAP_HASH_H

#include "snap_val.h"

#include <stddef.h>
#include <stdbool.h>

typedef struct {
  const char* key;
  SValue val;
} SEntry;

typedef struct {
  SEntry* entries;
  size_t capacity;
  size_t count;
} SnapHash;

void snap_hash_init(SnapHash* hash);
void snap_hash_destroy(SnapHash* hash);
bool snap_hash_put(SnapHash* hash, const char* key, SValue val);
bool snap_hash_delete(SnapHash* hash, const char* key);
bool snap_hash_get(SnapHash* hash, const char* key, SValue* val);

#endif
