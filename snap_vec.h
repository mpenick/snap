#ifndef SNAP_VEC_H
#define SNAP_VEC_H

#include <stdlib.h>

#ifndef snap_vec_realloc
#define snap_vec_realloc(ptr, size) realloc(ptr, size)
#endif

#ifndef snap_vec_free
#define snap_vec_free(ptr) free(ptr)
#endif

#define SNAP_VEC_FIELDS(type) \
  type *vitems;               \
  int vcapacity;              \
  int vsize

static inline void snap_vec_maybe_resize(int size, int* capacity,
                                         size_t num_bytes, void**items) {
  if (size >= *capacity) {
    *capacity *= ((*capacity < 4096) ? 4 : 2);
    *items = snap_vec_realloc(*items, num_bytes * *capacity);
  }
}

#define snap_vec_init(v, type, capacity) do {                    \
  if (capacity > 0) {                                            \
    (v)->vitems =                                                \
      (type *) snap_vec_realloc(NULL, capacity * sizeof(type));  \
  }                                                              \
  (v)->vcapacity = capacity;                                     \
  (v)->vsize = 0;                                                \
} while (0)

#define snap_vec_destroy(v) \
  snap_vec_free((v)->vitems)

#define snap_vec_check_size(v, type) \
  snap_vec_maybe_resize((v)->vsize, &(v)->vcapacity, \
                        sizeof(type), (void**)&(v)->vitems)

#define snap_vec_push(v, type) \
  (snap_vec_check_size(v, type), &(v)->vitems[(v)->vsize++])

#define snap_vec_back(v) \
  (v)->vitems[(v)->vsize - 1]

#define snap_vec_pop(v) \
  (((v)->vsize > 0) ? --(v)->vsize : (void)0)

#define snap_vec_foreach(v, item) \
  for (item = (v)->vitems; item < (v)->vitems + (v)->vsize; ++item)

#endif /* SNAP_VEC_H */
