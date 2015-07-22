#ifndef SNAP_HASH_H
#define SNAP_HASH_H

#include "snap.h"

#include <stddef.h>
#include <stdbool.h>

typedef struct {
  const char* key;
  Value value;
} Entry;

typedef struct {
  Entry* entries;
  size_t capacity;
  size_t count;
} Hash;

void hash_init(Hash* hash);
void hash_destroy(Hash* hash);
bool hash_put(Hash* hash, const char* key, Value* value);
bool hash_delete(Hash* hash, const char* key);
bool hash_get(Hash* hash, const char* key, Value* value);

#endif
