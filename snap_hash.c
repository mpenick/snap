#include "snap_hash.h"

#include "snap.h"

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define HASH_MIN_LOAD_FACTOR 0.2
#define HASH_MAX_LOAD_FACTOR 0.75
#define HASH_MIN_CAPACITY 8

int next_pow_of_2(int num) {
  int i = 2;
  while (i < num) i <<= 1;
  return i;
}

static SEntry* hash_lookup(SEntry* entries, int capacity, SValue key) {
  int i;
  int h = snap_hash(key);
  int mask = capacity - 1;
  for (i = 0; i < capacity; ++i) {
    SEntry *entry = &entries[(h + ((i + i * i) >> 1)) & mask];
    if (is_undef(entry->key) || snap_compare(key, entry->key) == 0) {
      return entry;
    }
  }
  assert(0);
  return NULL;
}

static void hash_resize(SnapHash* hash, int new_capacity) {
  int i;
  SEntry* new_entries;
  assert(hash->capacity >= HASH_MIN_CAPACITY);
  new_capacity = next_pow_of_2(new_capacity);
  if (new_capacity < HASH_MIN_CAPACITY) {
    new_capacity = HASH_MIN_CAPACITY;
  }
  new_entries = (SEntry*)calloc(new_capacity, sizeof(SEntry));
  for (i = 0; i < hash->capacity; ++i) {
    SEntry* entry = &hash->entries[i];
    if (!is_undef(entry->key)) {
      SEntry* new_entry = hash_lookup(new_entries, new_capacity, entry->key);
      *new_entry = *entry;
    }
  }
  free((void*)hash->entries);
  hash->entries = new_entries;
  hash->capacity = new_capacity;
}

void snap_hash_init(SnapHash* hash) {
  hash->entries = (SEntry*)calloc(HASH_MIN_CAPACITY, sizeof(SEntry));
  hash->capacity = HASH_MIN_CAPACITY;
  hash->count = 0;
}

void snap_hash_destroy(SnapHash* hash) {
  free((void*)hash->entries);
}

bool snap_hash_put(SnapHash* hash, SValue key, SValue val) {
  bool is_replaced;
  SEntry* entry = hash_lookup(hash->entries, hash->capacity, key);
  if (is_undef(entry->key)) {
    is_replaced = true;
  } else {
    hash->count++;
    if ((double)hash->count / hash->capacity > HASH_MAX_LOAD_FACTOR) {
      hash_resize(hash, hash->count / HASH_MIN_LOAD_FACTOR);
      // Need to find the new entry
      entry = hash_lookup(hash->entries, hash->capacity, key);
    }
    entry->key = key;
    is_replaced = false;
  }
  entry->val = val;
  return is_replaced;
}

bool snap_hash_delete(SnapHash* hash, SValue key) {
  SEntry* entry = hash_lookup(hash->entries, hash->capacity, key);
  if (!is_undef(entry->key)) return false;
  entry->key = create_undef();
  hash->count--;
  if ((double)hash->count / hash->capacity < HASH_MIN_LOAD_FACTOR &&
      hash->capacity > HASH_MIN_CAPACITY) {
    hash_resize(hash, hash->count / HASH_MAX_LOAD_FACTOR);
  }
  return true;
}

SValue* snap_hash_get(SnapHash* hash, SValue key) {
  SEntry* entry = hash_lookup(hash->entries, hash->capacity, key);
  if (!is_undef(entry->key)) return NULL;
  return &entry->val;
}
