#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define SEMI_SPACE_SIZE 8 * 1024 * 1024
#define STACK_SIZE 8 * 1024

enum {
  SNAP_ERROR_INDEX = 1,
  SNAP_ERROR_TYPE,
  SNAP_ERROR_PARAM,
  //SNAP_ERROR_,
  //SNAP_ERROR_,
  //SNAP_ERROR_,
  //SNAP_ERROR_,
};

uintptr_t round_to_alignment(uintptr_t value, uintptr_t alignment) {
  return (value + (alignment - 1)) & ~(alignment - 1);
}

typedef void* (*SnapAlloc)(void* data, void* ptr, size_t num_bytes);

#define snap_alloc(size) snap->alloc(snap->data, NULL, size)
#define snap_realloc(ptr, size) snap->alloc(snap->data, (void*)ptr, size)
#define snap_free(ptr) snap->alloc(snap->data, (void*)ptr, 0)

typedef struct SnapSemiSpace_ SnapSemiSpace;
typedef struct SnapObject_ SnapObject;
typedef struct SnapObjectFlags_ SnapObjectFlags;

typedef struct Snap_ Snap;
typedef struct SnapFrame_ SnapFrame;

typedef union SnapValue_ SnapValue;
typedef struct SnapPair_ SnapPair;

typedef struct SnapArray_ SnapArray;
typedef struct SnapCell_ SnapCell;
typedef struct SnapList_ SnapList;
typedef struct SnapHash_ SnapHash;
typedef struct SnapStr_ SnapStr;

struct SnapObjectFlags_ {
  int type          : 8;
  bool is_relocated : 1;
  bool is_tracing   : 1;
};

struct SnapObject_ {
  SnapObjectFlags flags;
  SnapObject* location;
};

struct SnapSemiSpace_ {
  char* current;
  char* start;
  char* end;
  size_t bytes_allocated;
};

union SnapValue_ {
  double f;
  union {
    void* p;
    struct {
#ifdef SNAP_BIG_ENDIAN
    uint32_t type;
    int32_t i;
#else
    int32_t i;
    uint32_t type;
#endif
    };
  };
};

enum {
  SNAP_TYPE_FALSE = 1,
  SNAP_TYPE_TRUE,
  SNAP_TYPE_NIL,
  SNAP_TYPE_FLOAT,
  SNAP_TYPE_INT,
  SNAP_TYPE_PTR,
  SNAP_TYPE_STR,
  SNAP_TYPE_CELL,
  SNAP_TYPE_ARRAY,
  SNAP_TYPE_LIST,
  SNAP_TYPE_HASH
};

#define snap_type(v) ((v).type > 0xfff00000 ? (((v).type >> 16) & 0x0f) : SNAP_TYPE_FLOAT)
#define snap_mktype(t) (0xfff00000 | (((uint32_t)(t)) << 16))
#define snap_is_gc(v) (snap_type(v) > SNAP_TYPE_INT)

#define snap_float(v) ((v).f)
#define snap_int(v) ((int32_t)v.i)
#define snap_ptr(v) ((void*)(((uintptr_t)(v).p) & 0x0000ffffffffffff))
#define snap_object(v) ((SnapObject*)snap_ptr(v))

#define snap_object_type(o) ((o)->flags.type)

static inline SnapValue snap_nil_value() {
  SnapValue v;
  v.type = SNAP_TYPE_NIL;
  v.i = 0;
  return v;
}

static inline SnapValue snap_bool_value(bool b) {
  SnapValue v;
  v.type = b ? SNAP_TYPE_TRUE : SNAP_TYPE_FALSE;
  v.i = 0;
  return v;
}

static inline SnapValue snap_int_value(int32_t i) {
  SnapValue v;
  v.type = snap_mktype(SNAP_TYPE_INT);
  v.i = i;
  return v;
}

static inline SnapValue snap_float_value(double f) {
  SnapValue v;
  if (f != f) {
    v.type = 0x7ff80000;
    v.i = 0;
  } else {
    v.f = f;
  }
  return v;
}

static inline SnapValue snap_ptr_value(void* p) {
  SnapValue v;
  v.type = snap_mktype(SNAP_TYPE_PTR);
  v.i = 0;
  v.p = (void*)((uintptr_t)p | (uintptr_t)v.p);
  return v;
}

static inline SnapValue snap_object_value(SnapObject* p) {
  SnapValue v;
  v.type = snap_mktype(p->flags.type);
  v.i = 0;
  v.p = (void*)((uintptr_t)p | (uintptr_t)v.p);
  return v;
}

struct SnapPair_ {
  SnapValue key;
  SnapValue value;
};

struct SnapArray_ {
  SnapObjectFlags flags;
  union {
    SnapObject* location;
    struct {
      size_t size;
      SnapValue data[0];
    };
  };
};

struct SnapCell_ {
  SnapObjectFlags flags;
  union {
    SnapObject* location;
    struct {
      SnapValue first;
      SnapValue rest;
    };
  };
};

struct SnapHash_ {
  SnapObjectFlags flags;
  union {
    SnapObject* location;
    struct {
      size_t size;
      size_t capacity;
      SnapPair data[0];
    };
  };
};

struct SnapList_ {
  SnapObjectFlags flags;
  union {
    SnapObject* location;
    struct {
      SnapArray* array;
      size_t size;
    };
  };
};

struct SnapStr_ {
  SnapObjectFlags flags;
  union {
    SnapObject* location;
    struct {
      size_t len;
      char data[0];
    };
  };
};

struct SnapFrame_ {
  SnapFrame* prev_frame;
  SnapValue* stack_base;
  SnapValue* stack_top;
};

struct Snap_ {
  SnapAlloc alloc;
  void* data;
  SnapSemiSpace to_space;
  SnapSemiSpace from_space;
  SnapFrame* frame;
  SnapValue* stack_base;
  SnapValue* stack_top;
  SnapValue* stack_end;
};

SnapObject* snapgc_alloc(Snap* snap, int type, size_t num_bytes);
void snapgc_mark_object(Snap* snap, SnapObject** object);
void snapgc_mark_value(Snap* snap, SnapValue* value);
void snapgc_collect(Snap* snap);

void snapgc_semispace_init(SnapSemiSpace* ss, char* memory, size_t size);
void* snapgc_semispace_allocate(SnapSemiSpace* ss, size_t num_bytes);
SnapObject* snapgc_semispace_first(SnapSemiSpace* ss);
SnapObject* snapgc_semispace_next(SnapSemiSpace* ss, SnapObject* pos);
void snapgc_semispace_reset(SnapSemiSpace* ss);

void snap_init(Snap* snap, SnapAlloc alloc, void* data);
void snap_cleanup(Snap* snap);
bool snap_push_frame(Snap* snap, SnapFrame* frame, size_t size);
void snap_pop_frame(Snap* snap, SnapFrame* frame);

SnapObject* snapgc_alloc(Snap* snap, int type, size_t num_bytes) {
  SnapObject* object = (SnapObject*)snapgc_semispace_allocate(&snap->from_space, num_bytes);
  if (object == NULL) {
    snapgc_collect(snap);
    object = (SnapObject*)snapgc_semispace_allocate(&snap->from_space, num_bytes);
    if (object == NULL) {
      assert(0);
    }
  }
  object->flags.type = type;
  object->flags.is_relocated = false;
  object->flags.is_tracing = false;
  object->location = NULL;
  return object;
}

size_t snapgc_alloc_size(SnapObject* object) {
  size_t size = 0;
  switch (snap_object_type(object)) {
  case SNAP_TYPE_ARRAY:
    size =  sizeof(SnapArray) + ((SnapArray*)object)->size * sizeof(SnapValue);
    break;
  case SNAP_TYPE_CELL:
    size =  sizeof(SnapCell);
    break;
  case SNAP_TYPE_HASH:
    size =  sizeof(SnapHash) + ((SnapHash*)object)->capacity * sizeof(SnapPair);
    break;
  case SNAP_TYPE_LIST:
    size =  sizeof(SnapList);
    break;
  case SNAP_TYPE_STR:
    size =  sizeof(SnapStr) + ((SnapStr*)object)->len;
    break;
  default:
    assert(0);
  }
  return size;
}

void snapgc_mark_children(Snap* snap, SnapObject* object) {
  switch (snap_object_type(object)) {
    case SNAP_TYPE_ARRAY:
    {
      SnapArray* array = (SnapArray*)object;
      for (size_t i = 0; i < array->size; ++i) {
        if (snap_is_gc(array->data[i])) {
          snapgc_mark_value(snap, &array->data[i]);
        }
      }
    }
    break;
    case SNAP_TYPE_CELL:
    {
      SnapCell* cell = (SnapCell*)object;
      if (snap_is_gc(cell->first)) {
        snapgc_mark_value(snap, &cell->first);
      }
      if (snap_is_gc(cell->rest)) {
        snapgc_mark_value(snap, &cell->rest);
      }
    }
    break;
    case SNAP_TYPE_HASH:
    {
      SnapHash* hash = (SnapHash*)object;
      for (size_t i = 0; i < hash->capacity; ++i) {
        if (snap_is_gc(hash->data[i].key)) {
          snapgc_mark_value(snap, &hash->data[i].key);
        }
        if (snap_is_gc(hash->data[i].value)) {
          snapgc_mark_value(snap, &hash->data[i].value);
        }
      }
    }
    break;
    case SNAP_TYPE_LIST:
      snapgc_mark_object(snap, (SnapObject**)&((SnapList*)object)->array);
    break;
    case SNAP_TYPE_STR:
      // No sub-objects
      break;
    default:
      assert(0);
  }
}

static inline SnapObject* snapgc_move_object(Snap* snap, SnapObject* object) {
  if (object->flags.is_relocated) {
    return object->location;
  } else {
    size_t num_bytes = snapgc_alloc_size(object);
    SnapObject* new_object = (SnapObject*)snapgc_semispace_allocate(&snap->to_space, num_bytes);
    memcpy(new_object, object, num_bytes);
    object->flags.is_relocated = true;
    object->location = new_object;
    return new_object;
  }
}

void snapgc_mark_value(Snap* snap, SnapValue* value) {
  *value = snap_object_value(snapgc_move_object(snap, snap_object(*value)));
}

void snapgc_mark_object(Snap* snap, SnapObject** object) {
  if (object == NULL) return;
  *object = snapgc_move_object(snap, *object);;
}

void snapgc_collect(Snap* snap) {
  for (SnapValue* v = snap->stack_base; v < snap->stack_top; ++v) {
    if (snap_is_gc(*v)) {
      snapgc_mark_value(snap, v);
    }
  }

  SnapObject* pos = snapgc_semispace_first(&snap->to_space);
  while (pos) {
    snapgc_mark_children(snap, pos);
    pos = snapgc_semispace_next(&snap->to_space, pos);
  }

  SnapSemiSpace temp = snap->from_space;
  snap->from_space = snap->to_space;
  snap->to_space = temp;
}

void snapgc_semispace_init(SnapSemiSpace* ss, char* memory, size_t size) {
  ss->bytes_allocated = 0;
  ss->start = memory;
  ss->current = memory;
  ss->end = ss->start + size - 1;
}

void* snapgc_semispace_allocate(SnapSemiSpace* ss, size_t num_bytes) {
  size_t allocated_num_bytes = round_to_alignment(num_bytes, sizeof(SnapObject));
  if (ss->current + allocated_num_bytes > ss->end) {
    return NULL;
  }
  void* mem = (void*)ss->current;
  ss->bytes_allocated += allocated_num_bytes;
  ss->current += allocated_num_bytes;
  return mem;
}

SnapObject* snapgc_semispace_first(SnapSemiSpace* ss) {
  if (ss->start == ss->current) return NULL;
  return (SnapObject*)ss->start;
}

SnapObject* snapgc_semispace_next(SnapSemiSpace* ss, SnapObject* pos) {
  char* next = (char*)(pos) + round_to_alignment(snapgc_alloc_size(pos), sizeof(SnapObject));
  if (next >= ss->current) return NULL;
  return (SnapObject*)next;
}

void snapgc_semispace_reset(SnapSemiSpace* ss) {
  ss->bytes_allocated = 0;
  ss->current = ss->start;
}

void snap_init(Snap* snap, SnapAlloc alloc, void* data) {
  snap->alloc = alloc;
  snap->data = data;
  snap->stack_base = (SnapValue*)snap_alloc(STACK_SIZE * sizeof(SnapValue));
  snap->stack_top = snap->stack_base;
  snap->stack_end = snap->stack_base + STACK_SIZE * sizeof(SnapValue);
  snap->frame = NULL;
  snapgc_semispace_init(&snap->from_space, (char*)snap_alloc(SEMI_SPACE_SIZE), SEMI_SPACE_SIZE);
  snapgc_semispace_init(&snap->to_space, (char*)snap_alloc(SEMI_SPACE_SIZE), SEMI_SPACE_SIZE);
}

void snap_cleanup(Snap* snap) {
  snap_free(snap->stack_base);
  snap_free(snap->from_space.start);
  snap_free(snap->to_space.start);
}

bool snap_push_frame(Snap* snap, SnapFrame* frame, size_t size) {
  if (snap->stack_top + size >= snap->stack_end) return false;

  frame->prev_frame = snap->frame;
  frame->stack_base = snap->stack_top;
  frame->stack_top = snap->stack_top + size;

  snap->frame = frame;

  SnapValue* stack = snap->stack_top;
  for (size_t i = 0; i < size; ++i) {
    stack[i] = snap_nil_value();
  }

  return true;
}

void snap_pop_frame(Snap* snap, SnapFrame* frame) {
  snap->frame = frame->prev_frame;
  snap->stack_top = frame->stack_base;
}

void snap_push(Snap* snap, SnapValue value) {
  *snap->stack_top = value;
  snap->stack_top++;
  assert(snap->stack_top <= snap->frame->stack_top);
}

void snap_pop(Snap* snap) {
  snap->stack_top--;
  assert(snap->stack_top >= snap->frame->stack_base);
}

int snap_at_index(Snap* snap, int index, SnapValue* value) {
  if (index < 0) {
    SnapValue* at_index = snap->stack_top + index;
    if (at_index < snap->frame->stack_base) {
      return SNAP_ERROR_INDEX;
    }
    *value = *at_index;
  } else {
    SnapValue* at_index = snap->stack_base + index;
    if (at_index >= snap->frame->stack_top) {
      return SNAP_ERROR_INDEX;
    }
    *value = *at_index;
  }
  return 0;
}

void snap_push_nil(Snap* snap) { snap_push(snap, snap_nil_value()); }
void snap_push_bool(Snap* snap, bool b) { snap_push(snap, snap_bool_value(b)); }
void snap_push_int(Snap* snap, int32_t i) { snap_push(snap, snap_int_value(i)); }
void snap_push_float(Snap* snap, int32_t f) { snap_push(snap, snap_float_value(f)); }
void snap_push_ptr(Snap* snap, void* p) { snap_push(snap, snap_ptr_value(p)); }

void snap_push_cell(Snap* snap) {
  SnapCell* cell = (SnapCell*)snapgc_alloc(snap,
                                           SNAP_TYPE_CELL,
                                           sizeof(SnapCell));
  cell->first = snap_nil_value();
  cell->rest = snap_nil_value();
  snap_push(snap, snap_object_value((SnapObject*)cell));
}

int snap_object_at_index(Snap* snap, int index, int type, SnapObject** object) {
  SnapValue value;
  int rc = snap_at_index(snap, index, &value);
  if (rc) return rc;
  if (snap_type(value) != type) {
    return SNAP_ERROR_TYPE;
  }
  *object = snap_object(value);
  return 0;
}

int snap_cell_set_first(Snap* snap, int index) {
  SnapCell* cell;
  int rc = snap_object_at_index(snap, index, SNAP_TYPE_CELL, (SnapObject**)&cell);
  if (rc) return rc;
  if (snap->stack_top == snap->frame->stack_base) {
    return SNAP_ERROR_PARAM;
  }
  cell->first = snap->stack_top[-1];
  snap_pop(snap);
  return 0;
}

int snap_cell_set_rest(Snap* snap, int index) {
  SnapCell* cell;
  int rc = snap_object_at_index(snap, index, SNAP_TYPE_CELL, (SnapObject**)&cell);
  if (rc) return rc;
  if (snap->stack_top == snap->frame->stack_base) {
    return SNAP_ERROR_PARAM;
  }
  cell->rest = snap->stack_top[-1];
  snap_pop(snap);
  return 0;
}

int snap_cell_get_first(Snap* snap, int index) {
  SnapCell* cell;
  int rc = snap_object_at_index(snap, index, SNAP_TYPE_CELL, (SnapObject**)&cell);
  if (rc) return rc;
  snap_push(snap, cell->first);
  return 0;
}

int snap_cell_get_rest(Snap* snap, int index) {
  SnapCell* cell;
  int rc = snap_object_at_index(snap, index, SNAP_TYPE_CELL, (SnapObject**)&cell);
  if (rc) return rc;
  snap_push(snap, cell->rest);
  return 0;
}

int snap_duplicate(Snap* snap) {
}

int snap_replace(Snap* snap, int index) {
  SnapValue value;
  int rc = snap_at_index(snap, index, *value);
  if (rc) return rc;
}

void snap_push_str_n(Snap* snap, const char* s, size_t len) {
  SnapStr* str = (SnapStr*)snapgc_alloc(snap,
                                        SNAP_TYPE_STR,
                                        sizeof(SnapStr) + len);
  memcpy(str->data, s, len);
  str->len = len;
  snap_push(snap, snap_object_value((SnapObject*)str));
}

void snap_push_str(Snap* snap, const char* s) {
  snap_push_str_n(snap, s, strlen(s));
}

void* alloc(void* data, void* ptr, size_t num_bytes) {
  (void)data;
  if (num_bytes == 0) {
    free(ptr);
    return NULL;
  } else {
    return realloc(ptr, num_bytes);
  }
}

int main() {
  Snap snap;
  snap_init(&snap, alloc, NULL);

  SnapFrame frame;
  snap_push_frame(&snap, &frame, 256);

  snap_pop_frame(&snap, &frame);
  return 0;
}
