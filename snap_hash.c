#include "snap_hash.h"

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define HASH_MIN_LOAD_FACTOR 0.2
#define HASH_MAX_LOAD_FACTOR 0.75
#define HASH_MIN_CAPACITY 8

size_t next_pow_of_2(size_t num) {
  size_t i = 2;
  while (i < num) i <<= 1;
  return i;
}

#ifdef BITS32
static uint32_t calc_hash(const void* data, size_t length) {
  uint32_t h = 0x811c9dc5;
  uint8_t* p = (uint8_t* )data;
  uint8_t* e = (uint8_t* )data + length;
  while (p < e) {
    h ^= (uint32_t)*p++;
    h *= 0x01000193;
  }
  return h;
}
#else
static uint64_t calc_hash(const void* data, size_t length) {
  uint64_t h = 0xcbf29ce484222325ULL;
  uint8_t* p = (uint8_t*)data;
  uint8_t* e = (uint8_t*)data + length;
  while (p < e) {
    h ^= (uint64_t)*p++;
    h *= 0x100000001b3ULL;
  }
  return h;
}
#endif

static SEntry* hash_lookup(SEntry* entries, size_t capacity, const char* key) {
  size_t h = calc_hash(key, strlen(key));
  size_t mask = capacity - 1;
  for (size_t i = 0; i < capacity; ++i) {
    SEntry *entry = &entries[(h + ((i + i * i) >> 1)) & mask];
    if (entry->key == NULL || strcmp(key, entry->key) == 0) {
      return entry;
    }
  }
  assert(0);
  return NULL;
}

static void hash_resize(SnapHash* hash, size_t new_capacity) {
  size_t i;
  SEntry* new_entries;
  assert(hash->capacity >= HASH_MIN_CAPACITY);
  new_capacity = next_pow_of_2(new_capacity);
  if (new_capacity < HASH_MIN_CAPACITY) {
    new_capacity = HASH_MIN_CAPACITY;
  }
  new_entries = (SEntry*)calloc(new_capacity, sizeof(SEntry));
  for (i = 0; i < hash->capacity; ++i) {
    SEntry* entry = &hash->entries[i];
    if (entry->key != NULL) {
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
  for (size_t i = 0; i < hash->capacity; ++i) {
    SEntry* entry = &hash->entries[i];
    if (entry->key != NULL) free((void*)entry->key);
  }
  free((void*)hash->entries);
}

bool snap_hash_put(SnapHash* hash, const char* key, SValue val) {
  bool is_replaced;
  SEntry* entry = hash_lookup(hash->entries, hash->capacity, key);
  if (entry->key) {
    is_replaced = true;
  } else {
    hash->count++;
    if ((double)hash->count / hash->capacity > HASH_MAX_LOAD_FACTOR) {
      hash_resize(hash, hash->count / HASH_MIN_LOAD_FACTOR);
      // Need to find the new entry
      entry = hash_lookup(hash->entries, hash->capacity, key);
    }
    entry->key = strdup(key);
    is_replaced = false;
  }
  entry->val = val;
  return is_replaced;
}

bool snap_hash_delete(SnapHash* hash, const char* key) {
  SEntry* entry = hash_lookup(hash->entries, hash->capacity, key);
  if (!entry->key) return false;
  free((void*)entry->key);
  entry->key = NULL;
  hash->count--;
  if ((double)hash->count / hash->capacity < HASH_MIN_LOAD_FACTOR &&
      hash->capacity > HASH_MIN_CAPACITY) {
    hash_resize(hash, hash->count / HASH_MAX_LOAD_FACTOR);
  }
  return true;
}

SValue* snap_hash_get(SnapHash* hash, const char* key) {
  SEntry* entry = hash_lookup(hash->entries, hash->capacity, key);
  if (!entry->key) return NULL;
  return &entry->val;
}
