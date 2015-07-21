#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define HASH_MIN_LOAD_FACTOR 0.2
#define HASH_MAX_LOAD_FACTOR 0.75
#define HASH_MIN_CAPACITY 8

typedef struct {
  const char* key;
  Value value;
} Entry;

typedef struct {
  Entry* entries;
  size_t capacity;
  size_t count;
} Hash;

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

static inline size_t next_pow_of_2(size_t num) {
  size_t i = 2;
  while (i < num) i <<= 1;
  return i;
}

static Entry* hash_lookup(Entry* entries, size_t capacity, const char* key) {
  size_t h = calc_hash(key, strlen(key));
  size_t mask = capacity - 1;
  for (size_t i = 0; i < capacity; ++i) {
    Entry *entry = &entries[(h + ((i + i * i) >> 1)) & mask];
    if (entry->key == NULL || strcmp(key, entry->key) == 0) {
      return entry;
    }
  }
  assert(0);
  return NULL;
}

static void hash_resize(Hash* hash, size_t new_capacity) {
  size_t i;
  Entry* new_entries;
  assert(hash->capacity >= HASH_MIN_CAPACITY);
  new_capacity = next_pow_of_2(new_capacity);
  if (new_capacity < HASH_MIN_CAPACITY) {
    new_capacity = HASH_MIN_CAPACITY;
  }
  new_entries = (Entry*)calloc(new_capacity, sizeof(Entry));
  for (i = 0; i < hash->capacity; ++i) {
    Entry* entry = &hash->entries[i];
    if (entry->key != NULL) {
      Entry* new_entry = hash_lookup(new_entries, new_capacity, entry->key);
      *new_entry = *entry;
    }
  }
  free((void*)hash->entries);
  hash->entries = new_entries;
  hash->capacity = new_capacity;
}

void hash_init(Hash* hash) {
  hash->entries = (Entry*)calloc(HASH_MIN_CAPACITY, sizeof(Entry));
  hash->capacity = HASH_MIN_CAPACITY;
  hash->count = 0;
}

void hash_destroy(Hash* hash) {
  for (size_t i = 0; i < hash->capacity; ++i) {
    Entry* entry = &hash->entries[i];
    if (entry->key != NULL) free((void*)entry->key);
  }
  free((void*)hash->entries);
}

bool hash_put(Hash* hash, const char* key, Value* value) {
  bool is_replaced;
  Entry* entry = hash_lookup(hash->entries, hash->capacity, key);
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
  entry->value = *value;
  return is_replaced;
}

bool hash_delete(Hash* hash, const char* key) {
  Entry* entry = hash_lookup(hash->entries, hash->capacity, key);
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

bool hash_get(Hash* hash, const char* key, Value* value) {
  Entry* entry = hash_lookup(hash->entries, hash->capacity, key);
  if (!entry->key) return false;
  *value = entry->value;
  return true;
}

int main() {
  Hash h;

  const char** key;
  const char* keys[] = { "abcdefghijkl", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", NULL };

  hash_init(&h);

  int i = 0;
  for (key = keys; *key; ++key) {
    Value v;
    v.i = i++;
    hash_put(&h, *key, &v);
  }

  for (key = keys; *key; ++key) {
    Value v;
    hash_get(&h, *key, &v);
    printf("%s %d\n", *key, v.i);
  }

  for (key = keys; *key; ++key) {
    Value v;
    hash_delete(&h, *key);
  }

  hash_destroy(&h);
}
