#ifndef MSTRUCTS_H
#define MSTRUCTS_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Hash table */

#ifndef mhash_malloc
#define mhash_malloc(size) malloc(size)
#endif

#ifndef mhash_free
#define mhash_free(ptr) free(ptr)
#endif

#define MHASH_MIN_CAPACITY 2
#define MHASH_MIN_LOAD_FACTOR 0.2
#define MHASH_MAX_LOAD_FACTOR 0.75

#define MHASH_ENTRY_FIELDS(ktype) ktype hkey

#define MHASH_FIELDS(type, ktype)                                              \
  type *hentries;                                                              \
  int hcapacity;                                                               \
  int hcount;                                                                  \
  ktype hempty;                                                                \
  ktype hdeleted

#define MHASH_FUNC(type, ktype) int mhash_func_name_(type)(const ktype key)

#define MHASH_EQUALS_FUNC(type, ktype)                                         \
  int mhash_equals_func_name_(type)(const ktype key1, ktype key2)

static inline unsigned mhash_fnv1a(const void *data, size_t length) {
  unsigned h = 0x811c9dc5;
  unsigned char *p = (unsigned char *)data;
  unsigned char *e = (unsigned char *)data + length;
  while (p < e) {
    h ^= *p++;
    h *= 0x01000193;
  }
  return h;
}

#define mhash_is_empty(h, type, entry)                                         \
  mhash_equals_func_name_(type)((entry)->hkey, (h)->hempty)

#define mhash_is_deleted(h, type, entry)                                       \
  mhash_equals_func_name_(type)((entry)->hkey, (h)->hdeleted)

#define mhash_is_vacant_(h, type, entry, allow_deleted)                        \
  (mhash_is_empty(h, type, entry) ||                                           \
   (allow_deleted && mhash_is_deleted(h, type, entry)))

#define mhash_func_name_(type) mhash_##type##_func_
#define mhash_equals_func_name_(type) mhash_##type##_equals_func_

#define mhash_lookup_(h, type, entries, capacity, key, entry, action,          \
                      allow_deleted)                                           \
  do {                                                                         \
    int j;                                                                     \
    int hv = mhash_func_name_(type)(key);                                      \
    int mask = capacity - 1;                                                   \
    for (j = 0; j < capacity; ++j) {                                           \
      type *e = &(entries)[(hv + ((j + j * j) >> 1)) & mask];                  \
      if (mhash_is_vacant_(h, type, e, allow_deleted) ||                       \
          mhash_equals_func_name_(type)(key, e->hkey)) {                       \
        action(e, entry);                                                      \
        break;                                                                 \
      }                                                                        \
    }                                                                          \
  } while (0)

#define mhash_find_action_(found, entry) entry = found
#define mhash_resize_action_(found, entry) *found = entry

#define mhash_change_capacity_(h, type, capacity)                              \
  do {                                                                         \
    int new_capacity = MHASH_MIN_CAPACITY;                                     \
    int next_capacity = capacity;                                              \
    type *new_entries;                                                         \
    while (new_capacity < next_capacity)                                       \
      new_capacity <<= 1;                                                      \
    new_entries = mhash_malloc(sizeof(type) * new_capacity);                   \
    for (i = 0; i < new_capacity; ++i) {                                       \
      new_entries[i].hkey = (h)->hempty;                                       \
    }                                                                          \
    for (i = 0; i < (h)->hcapacity; ++i) {                                     \
      if (!mhash_is_vacant_(h, type, &(h)->hentries[i], 1)) {                  \
        mhash_lookup_(h, type, new_entries, new_capacity,                      \
                      (h)->hentries[i].hkey, (h)->hentries[i],                 \
                      mhash_resize_action_, 1);                                \
      }                                                                        \
    }                                                                          \
    mhash_free((h)->hentries);                                                 \
    (h)->hentries = new_entries;                                               \
    (h)->hcapacity = new_capacity;                                             \
  } while (0)

#define mhash_init(h, type, empty, deleted, capacity)                          \
  do {                                                                         \
    int i;                                                                     \
    (h)->hcapacity = MHASH_MIN_CAPACITY;                                       \
    while ((h)->hcapacity < capacity)                                          \
      (h)->hcapacity <<= 1;                                                    \
    (h)->hcount = 0;                                                           \
    (h)->hentries = mhash_malloc(sizeof(type) * (h)->hcapacity);               \
    (h)->hempty = empty;                                                       \
    (h)->hdeleted = deleted;                                                   \
    for (i = 0; i < (h)->hcapacity; ++i) {                                     \
      (h)->hentries[i].hkey = empty;                                           \
    }                                                                          \
  } while (0)

#define mhash_destroy(h) mhash_free((h)->hentries);

#define mhash_find(h, type, key, entry, allow_deleted)                         \
  do {                                                                         \
    entry = NULL;                                                              \
    mhash_lookup_(h, type, (h)->hentries, (h)->hcapacity, key, entry,          \
                  mhash_find_action_, allow_deleted);                          \
  } while (0)

#define mhash_resize(h, type, capacity)                                        \
  do {                                                                         \
    int i;                                                                     \
    if (capacity < (h)->hcapacity &&                                           \
        (double)(h)->hcount / (h)->hcapacity < MHASH_MIN_LOAD_FACTOR) {        \
      mhash_change_capacity_(h, type, (h)->hcount / MHASH_MAX_LOAD_FACTOR);    \
    } else if (capacity >= (h)->hcapacity &&                                   \
               (double)(h)->hcount / (h)->hcapacity > MHASH_MAX_LOAD_FACTOR) { \
      mhash_change_capacity_(h, type, (h)->hcount / MHASH_MIN_LOAD_FACTOR);    \
    }                                                                          \
  } while (0)

#define mhash_add(h, type)                                                     \
  do {                                                                         \
    (h)->hcount++;                                                             \
    mhash_resize(h, type, (h)->hcapacity);                                     \
  } while (0)

#define mhash_delete(h, type, entry)                                           \
  do {                                                                         \
    (h)->hcount--;                                                             \
    entry->hkey = (h)->hdeleted;                                               \
  } while (0)

#define mhash_foreach(h, entry)                                                \
  for (entry = (h)->hentries; entry < (h)->hentries + (h)->hcapacity; ++entry)

/* Linked list */

typedef struct MList_ {
  struct MList_ *prev;
  struct MList_ *next;
} MList;

#define mlist_entry(type, ptr, member)                                         \
  ((type *)(((char *)ptr) - (uintptr_t) & ((type *)0)->member))

#define mlist_foreach(list)                                                    \
  for (pos = (list)->next; pos != (list); pos = pos->next)

#define mlist_is_empty(list) (list)->next == (list);

static inline void mlist_init(MList *list) { list->next = list->prev = list; }

static inline void mlist_copy(MList *src, MList *dst) {
  src->next->prev = dst;
  src->prev->next = dst;
  dst->next = src->next;
  dst->prev = src->prev;
}

static inline void mlist_add(MList *prev, MList *next, MList *entry) {
  prev->next = entry;
  entry->prev = prev;
  entry->next = next;
  next->prev = entry;
}

static inline void mlist_remove(MList *node) {
  node->prev->next = node->next;
  node->next->prev = node->prev;
  node->next = NULL;
  node->prev = NULL;
}

static inline void mlist_append(MList *list, MList *node) {
  mlist_add(list->prev, list, node);
}

static inline void mlist_prepend(MList *list, MList *node) {
  mlist_add(list, list->next, node);
}

static inline int mlist_forward_count(MList *first, MList *last) {
  int count = 0;
  MList *curr = first;
  while (curr != last) {
    count++;
    curr = curr->next;
  }
  return count++;
}

static inline int mlist_backward_count(MList *first, MList *last) {
  int count = 0;
  MList *curr = first;
  while (curr != last) {
    count++;
    curr = curr->prev;
  }
  return count++;
}

/* Vector */

#ifndef mvec_realloc
#define mvec_realloc(ptr, size) realloc(ptr, size)
#endif

#ifndef mvec_free
#define mvec_free(ptr) free(ptr)
#endif

#define MVEC_FIELDS(type)                                                      \
  type *vitems;                                                                \
  int vcapacity;                                                               \
  int vsize

static inline void mvec_maybe_resize_(int size, int *capacity, size_t num_bytes,
                                      void **items) {
  if (size >= *capacity) {
    *capacity *= ((*capacity < 4096) ? 4 : 2);
    *items = mvec_realloc(*items, num_bytes * *capacity);
  }
}

#define mvec_init(v, type, capacity)                                           \
  do {                                                                         \
    if (capacity > 0) {                                                        \
      (v)->vitems = (type *)mvec_realloc(NULL, capacity * sizeof(type));       \
    }                                                                          \
    (v)->vcapacity = capacity;                                                 \
    (v)->vsize = 0;                                                            \
  } while (0)

#define mvec_destroy(v) mvec_free((v)->vitems)

#define mvec_check_size(v, type)                                               \
  mvec_maybe_resize_((v)->vsize, &(v)->vcapacity, sizeof(type),                \
                     (void **)&(v)->vitems)

#define mvec_push(v, type)                                                     \
  (mvec_check_size(v, type), &(v)->vitems[(v)->vsize++])

#define mvec_back(v) (v)->vitems[(v)->vsize - 1]

#define mvec_pop(v) (((v)->vsize > 0) ? --(v)->vsize : (void)0)

#define mvec_foreach(v, item)                                                  \
  for (item = (v)->vitems; item < (v)->vitems + (v)->vsize; ++item)

#endif /* MSTRUCTS_H */
