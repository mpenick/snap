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
  SNAP_ERROR_PARAM
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
typedef struct SnapCode_ SnapCode;
typedef struct SnapList_ SnapList;
typedef struct SnapHash_ SnapHash;
typedef struct SnapStr_ SnapStr;

struct SnapSemiSpace_ {
  char* current;
  char* start;
  char* end;
  size_t bytes_allocd;
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

struct SnapObjectFlags_ {
  int type          : 8;
  bool is_tracing   : 1;
};

#define SNAP_OBJECT_FIELDS \
  SnapObjectFlags flags;   \
  SnapObject* forward;


struct SnapObject_ {
  SNAP_OBJECT_FIELDS
};

enum {
  SNAP_TYPE_FALSE = 1,
  SNAP_TYPE_TRUE,
  SNAP_TYPE_NIL,
  SNAP_TYPE_FLOAT,
  SNAP_TYPE_INT,
  SNAP_TYPE_PTR,
  SNAP_TYPE_STR,
  SNAP_TYPE_DATA,
  SNAP_TYPE_CELL,
  SNAP_TYPE_ARRAY,
  SNAP_TYPE_LIST,
  SNAP_TYPE_HASH,
  SNAP_TYPE_CODE,
  SNAP_TYPE_FUNC,
};

#define snap_type(v) ((v).type > 0xfff00000 ? (((v).type >> 16) & 0x0f) : SNAP_TYPE_FLOAT)
#define snap_mktype(t) (0xfff00000 | (((uint32_t)(t)) << 16))
#define snap_is_gc(v) (snap_type(v) > SNAP_TYPE_INT)

#define snap_float(v) ((v).f)
#define snap_int(v) ((int32_t)v.i)
#define snap_ptr(v) ((void*)(((uintptr_t)(v).p) & 0x0000ffffffffffff))
#define snap_object(v) ((SnapObject*)snap_ptr(v))

#define snap_object_type(o) ((o)->flags.type)

#define snap_is_forwared(o) ((SnapObject*)(o) != (o)->forward)
#define snap_rb(o, type) ((type*)(o)->forward)
#define snap_wb(o, type, field, value) ((type*)(o)->forward)->field = value

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
  SNAP_OBJECT_FIELDS
  size_t size;
  SnapValue data[0];
};

struct SnapCell_ {
  SNAP_OBJECT_FIELDS
  SnapValue first;
  SnapValue rest;
};

struct SnapCode_ {
  SNAP_OBJECT_FIELDS
  SnapStr* name;
  int num_params;
  int num_locals;
  size_t insts_capacity;
  size_t insts_size;
  uint32_t insts[0];
};

struct SnapHash_ {
  SNAP_OBJECT_FIELDS
  size_t size;
  size_t capacity;
  SnapPair data[0];
};

struct SnapList_ {
  SNAP_OBJECT_FIELDS
  SnapArray* array;
  size_t size;
};

struct SnapStr_ {
  SNAP_OBJECT_FIELDS
  size_t len;
  char data[0];

};

struct SnapBytes_ {
  SNAP_OBJECT_FIELDS
  size_t size;
  char data[0];
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
  /*
  SnapCode** current;
  int32_t**  current_insts;
  */
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
void* snapgc_semispace_alloc(SnapSemiSpace* ss, size_t num_bytes);
SnapObject* snapgc_semispace_first(SnapSemiSpace* ss);
SnapObject* snapgc_semispace_next(SnapSemiSpace* ss, SnapObject* pos);
void snapgc_semispace_reset(SnapSemiSpace* ss);

void snap_init(Snap* snap, SnapAlloc alloc, void* data);
void snap_cleanup(Snap* snap);
bool snap_push_frame(Snap* snap, SnapFrame* frame, size_t size);
void snap_pop_frame(Snap* snap, SnapFrame* frame);

SnapObject* snapgc_alloc(Snap* snap, int type, size_t num_bytes) {
  SnapObject* object = (SnapObject*)snapgc_semispace_alloc(&snap->to_space, num_bytes);
  if (object == NULL) {
    snapgc_collect(snap);
    object = (SnapObject*)snapgc_semispace_alloc(&snap->to_space, num_bytes);
    if (object == NULL) {
      assert(0);
    }
  }
  object->flags.type = type;
  object->flags.is_tracing = false;
  object->forward = object;
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
  if (snap_is_forwared(object)) {
    return object->forward;
  } else {
    size_t num_bytes = snapgc_alloc_size(object);
    SnapObject* new_object = (SnapObject*)snapgc_semispace_alloc(&snap->to_space, num_bytes);
    memcpy(new_object, object, num_bytes);
    object->forward = new_object;
    new_object->forward = new_object;
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
  SnapSemiSpace temp = snap->from_space;
  snap->from_space = snap->to_space;
  snap->to_space = temp;

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
}

void snapgc_semispace_init(SnapSemiSpace* ss, char* memory, size_t size) {
  ss->bytes_allocd = 0;
  ss->start = memory;
  ss->current = memory;
  ss->end = ss->start + size - 1;
}

void* snapgc_semispace_alloc(SnapSemiSpace* ss, size_t num_bytes) {
  size_t allocd_num_bytes = round_to_alignment(num_bytes, sizeof(SnapObject));
  if (ss->current + allocd_num_bytes > ss->end) {
    return NULL;
  }
  void* mem = (void*)ss->current;
  ss->bytes_allocd += allocd_num_bytes;
  ss->current += allocd_num_bytes;
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
  ss->bytes_allocd = 0;
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

SnapValue* snap_at_index(Snap* snap, int index) {
  SnapValue* value = NULL;
  if (index < 0) {
    value = snap->stack_top + index;
    if (value < snap->frame->stack_base) {
      return NULL;
    }
  } else {
    value = snap->stack_base + index;
    if (value >= snap->frame->stack_top) {
      return NULL;
    }
  }
  return value;
}

int snap_to_abs_index(Snap* snap, int index) {
  if (index < 0) {
    return (snap->stack_top + index) - snap->frame->stack_base;
  }
  return index;
}

int snap_duplicate(Snap* snap) {
  if (snap->stack_top == snap->frame->stack_base) {
    return SNAP_ERROR_PARAM;
  }
  snap_push(snap, snap->stack_top[-1]);
  return 0;
}

int snap_replace(Snap* snap, int index) {
  SnapValue* value = snap_at_index(snap, index);
  if (!value) return SNAP_ERROR_INDEX;
  if (snap->stack_top == snap->frame->stack_base) {
    return SNAP_ERROR_PARAM;
  }
  *value = snap->stack_top[-1];
  snap_pop(snap);
  return 0;
}


void snap_push_nil(Snap* snap) { snap_push(snap, snap_nil_value()); }
void snap_push_bool(Snap* snap, bool b) { snap_push(snap, snap_bool_value(b)); }
void snap_push_int(Snap* snap, int32_t i) { snap_push(snap, snap_int_value(i)); }
void snap_push_float(Snap* snap, int32_t f) { snap_push(snap, snap_float_value(f)); }
void snap_push_ptr(Snap* snap, void* p) { snap_push(snap, snap_ptr_value(p)); }

void snap_push_str(Snap* snap, const char* s);
void snap_push_str_n(Snap* snap, const char* s, size_t len);

static inline int snap_object_at_index(Snap* snap, int index, int type, SnapObject** object) {
  SnapValue* value = snap_at_index(snap, index);
  if (!value) return SNAP_ERROR_INDEX;
  if (snap_type(*value) != type) {
    return SNAP_ERROR_TYPE;
  }
  *object = snap_object(*value);
  return 0;
}

static inline SnapObject* snap_get_object(Snap* snap, int index, int type) {
  SnapObject* object;
  int rc = snap_object_at_index(snap, index, type, (SnapObject**)&object);
  (void)rc;
  assert(!rc);
  return object;
}

#define SNAP_GET_OBJECT_AT_INDEX(type, typec) \
  type* object; \
  int rc = snap_object_at_index(snap, index, typec, (SnapObject**)&object); \
  if (rc) return rc; \

#define SNAP_GET_FIELD_VALUE(name, type, typec, field) \
int name(Snap* snap, int index) { \
  SNAP_GET_OBJECT_AT_INDEX(type, typec) \
  snap_push(snap, snap_rb(object, type)->field); \
  return 0; \
}

#define SNAP_SET_FIELD_VALUE(name, type, typec, field) \
int name(Snap* snap, int index) { \
  SNAP_GET_OBJECT_AT_INDEX(type, typec) \
  if (snap->stack_top == snap->frame->stack_base) { \
    return SNAP_ERROR_PARAM; \
  } \
  snap_wb(object, type, field, snap->stack_top[-1]); \
  snap_pop(snap); \
  return 0; \
}

#define SNAP_GET_FIELD_OBJECT(name, type, typec, field) \
int name(Snap* snap, int index) { \
  SNAP_GET_OBJECT_AT_INDEX(type, typec) \
  snap_push(snap, snap_object_value((SnapObject*)snap_rb(object, type)->field)); \
  return 0; \
}

#define SNAP_SET_FIELD_OBJECT(name, type, typec, field, field_type, field_typec) \
int name(Snap* snap, int index) { \
  SNAP_GET_OBJECT_AT_INDEX(type, typec) \
  if (snap->stack_top == snap->frame->stack_base) { \
    return SNAP_ERROR_PARAM; \
  } \
  SnapValue value = snap->stack_top[-1]; \
  if (snap_type(value) != field_typec) { \
    return SNAP_ERROR_TYPE; \
  } \
  snap_wb(object, type, field, (field_type*)snap_object(value)); \
  snap_pop(snap); \
  return 0; \
}

#define SNAP_GET_FIELD_PRIMITIVE(name, type, typec, field, field_type) \
field_type name(Snap* snap, int index) { \
  type* object = (type*)snap_get_object(snap, index, typec); \
  return snap_rb(object, type)->field; \
}

#define SNAP_SET_FIELD_PRIMITIVE(name, type, typec, field, field_type) \
void name(Snap* snap, int index, field_type value) { \
  type* object = (type*)snap_get_object(snap, index, typec); \
  snap_wb(object, type, field, value); \
}

void snap_push_cell(Snap* snap) {
  SnapCell* cell =
    (SnapCell*)snapgc_alloc(snap, SNAP_TYPE_CELL, sizeof(SnapCell));
  cell->first = snap_nil_value();
  cell->rest = snap_nil_value();
  snap_push(snap, snap_object_value((SnapObject*)cell));
}

SNAP_GET_FIELD_VALUE(snap_cell_get_first, SnapCell, SNAP_TYPE_CELL, first)
SNAP_GET_FIELD_VALUE(snap_cell_get_rest, SnapCell, SNAP_TYPE_CELL, rest)
SNAP_SET_FIELD_VALUE(snap_cell_set_first, SnapCell, SNAP_TYPE_CELL, first)
SNAP_SET_FIELD_VALUE(snap_cell_set_rest, SnapCell, SNAP_TYPE_CELL, rest)

void snap_push_code(Snap* snap,
                    size_t insts_capacity) {
  SnapCode* code =
    (SnapCode*)snapgc_alloc(snap,
                            SNAP_TYPE_CODE,
                            sizeof(SnapCode) +
                            sizeof(int32_t) * insts_capacity);
  code->name = NULL;
  code->num_locals = 0;
  code->num_params = 0;
  code->insts_capacity = insts_capacity;
  code->insts_size = 0;
  snap_push(snap, snap_object_value((SnapObject*)code));
}

SNAP_GET_FIELD_OBJECT(snap_code_get_name, SnapCode, SNAP_TYPE_CODE, name)
SNAP_SET_FIELD_OBJECT(snap_code_set_name, SnapCode, SNAP_TYPE_CODE, name, SnapStr, SNAP_TYPE_STR)

SNAP_GET_FIELD_PRIMITIVE(snap_code_get_num_locals, SnapCode, SNAP_TYPE_CODE, num_locals, int)
SNAP_SET_FIELD_PRIMITIVE(snap_code_set_num_locals, SnapCode, SNAP_TYPE_CODE, num_locals, int)
SNAP_GET_FIELD_PRIMITIVE(snap_code_get_num_params, SnapCode, SNAP_TYPE_CODE, num_params, int)
SNAP_SET_FIELD_PRIMITIVE(snap_code_set_num_params, SnapCode, SNAP_TYPE_CODE, num_params, int)

SNAP_GET_FIELD_PRIMITIVE(snap_code_get_size, SnapCode, SNAP_TYPE_CODE, insts_size, size_t)
SNAP_GET_FIELD_PRIMITIVE(snap_code_get_capacity, SnapCode, SNAP_TYPE_CODE, insts_capacity, size_t)

uint32_t snap_code_get_inst(Snap* snap, int index, size_t pos) {
  SnapCode* code = snap_rb(snap_get_object(snap, index, SNAP_TYPE_CODE), SnapCode);
  assert(pos < code->insts_size);
  return code->insts[pos];
}

void snap_code_set_inst(Snap* snap, int index, size_t pos, uint32_t inst) {
  SnapCode* code = snap_rb(snap_get_object(snap, index, SNAP_TYPE_CODE), SnapCode);
  assert(pos < code->insts_size);
  code->insts[pos] = inst;
}

void snap_code_append(Snap* snap, int index, uint32_t inst) {
  SnapCode* code = snap_rb(snap_get_object(snap, index, SNAP_TYPE_CODE), SnapCode);
  if (code->insts_size + 1 > code->insts_capacity) {
    index = snap_to_abs_index(snap, index);
    snap_push_code(snap, code->insts_capacity * (code->insts_capacity) > 16384 ? 2 : 4);
    SnapCode* new_code = (SnapCode*)snap_object(snap->stack_top[-1]);

    code = snap_rb(code, SnapCode);
    new_code->num_locals = code->num_locals;
    new_code->num_params = code->num_params;
    new_code->insts_size = code->insts_size;
    memcpy(new_code->insts, code->insts, sizeof(uint32_t) * code->insts_size);

    snap_replace(snap, index);
    code = new_code;
  }
  code->insts[code->insts_size++] = inst;
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
