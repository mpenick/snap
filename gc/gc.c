#include <assert.h>

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "mstructs.h"

#define SNAP_SEMI_SPACE_CHUNK_SIZE (256 * 1024)

#define SNAP_YOUNG_GEN_MAX_SIZE (2 * 1024 * 1024)
#define SNAP_YOUNG_GEN_MAX_OBJECT_SIZE 64 * 1024

// Marked == Moved

#define PTRMASK (~((size_t)0)) - 1)
#define RB(obj) (obj & 0x1) ?

typedef struct Snap_ Snap;

typedef struct SCode_ SCode;
typedef struct SObject_ SObject;
typedef struct SValue_ SValue;

typedef long SnapInt;
typedef double SnapFloat;
typedef void (*SCFunc)(Snap* snap, const SValue* args, int num_args, SValue* result);

#define is_obj_p(v) ((v)->type > STYPE_CFUNC)

#define STYPE_MAPPING(XX) \
  XX(STYPE_UNDEF, 0, "undef") \
  XX(STYPE_DELETED, 1, "deleted") \
  XX(STYPE_NIL, 2, "nil") \
  XX(STYPE_BOOL, 3, "bool") \
  XX(STYPE_INT, 4, "int") \
  XX(STYPE_FLOAT, 5, "float") \
  XX(STYPE_FORM, 6, "form") \
  XX(STYPE_CFUNC, 7, "cfunc") \
  XX(STYPE_SYM, 8, "sym") \
  XX(STYPE_STR, 9, "str") \
  XX(STYPE_ERR, 10, "err") \
  XX(STYPE_CONS, 11, "cons") \
  XX(STYPE_ARR, 12, "array") \
  XX(STYPE_INST, 13, "inst") \
  XX(STYPE_SCOPE, 14, "scope") \
  XX(STYPE_CODE_GEN, 15, "codegen") \
  XX(STYPE_CODE, 16, "code") \
  XX(STYPE_KEY, 17, "key") \
  XX(STYPE_TEMPSTR, 18, "tempstr") \
  XX(STYPE_CLOSURE, 19, "closure") \
  XX(STYPE_CLOSED_DESC, 20, "closeddesc")

enum {
#define XX(type, id, name) type,
  STYPE_MAPPING(XX)
#undef XX
};

struct SValue_ {
  uint8_t type;
  union {
    bool b;
    SnapInt i;
    SnapFloat f;
    SObject* o;
    SCFunc c;
  };
};

typedef struct SnapVec_ {
  MVEC_FIELDS(SValue);
} SnapVec;

typedef struct {
  MHASH_ENTRY_FIELDS(SValue);
  SValue val;
} SnapHashEntry;

typedef struct {
  MHASH_FIELDS(SnapHashEntry, SValue);
} SnapHash;

#define SOBJECT_FIELDS \
  uint8_t type;        \
  uint8_t flags;       \
  uint16_t size;

typedef struct SSymStr_ {
  SOBJECT_FIELDS
  size_t len;
  char data[0];
} SSymStr;

typedef struct SKeyword_ {
  SOBJECT_FIELDS
  int id;
  size_t len;
  char data[0];
} SKeyword;

typedef struct SArr_ {
  SOBJECT_FIELDS
  int len;
  SValue data[0];
} SArr;

typedef struct SStuff_ {
  SOBJECT_FIELDS
  int stuff;
} SStuff;

typedef struct SObject_ {
  SOBJECT_FIELDS
} SObject;

typedef struct SObjectRelocated_ {
  SOBJECT_FIELDS
  SObject* location;
} SObjectRelocated;

SValue create_obj_(SObject* o) {
  SValue val;
  val.type = o->type;
  val.o = o;
  return val;
}

#define create_obj(o) create_obj_((SObject*)o)

typedef struct SnapStack_ {
  struct SnapStack_* next;
  SObject*** objs;
  size_t count;
} SnapStack;

typedef struct SnapSemiSpace_ {
  char* pos;
  char* end;
  struct SnapSemiSpace_* next;
  char begin[0];
} SnapSemiSpace;

struct Snap_ {
  SnapSemiSpace* to_space;
  SnapSemiSpace* from_space;
  size_t young_gen_allocated;
  SnapStack* roots;
};

static SObject* gc_semi_space_allocate(SnapSemiSpace* space, uint8_t type, size_t size);
static SnapSemiSpace* gc_semi_space_new(size_t size, SnapSemiSpace* next);

enum {
  GC_YOUNG_GEN,
  GC_MATURE_GEN
};

static inline size_t round_to_alignment(size_t value, size_t alignment) {
   return (value + (alignment - 1)) & ~(alignment - 1);
}

#define gc_set_relocated_flag(obj) \
  ((obj)->flags |= 0x1)

#define gc_relocated_flag(obj) \
  ((obj)->flags & 0x1)

#define gc_set_gen_flag(obj, gen) \
  ((obj)->flags |= (((gen) & 0x3) << 1))

#define gc_gen_flag(obj) \
  (((obj)->flags >> 1) & 0x3)

#define gc_size(obj) ((obj)->size > 0 ? (obj)->size : gc_size_slow(obj))

static size_t gc_size_slow(SObject* obj) {
  assert(false && "Not implemented");
  return 0;
}

static void gc_mark(Snap* snap, SObject** ptr) {
  SObject* obj = *ptr;
  if (!obj) return;
  if (gc_gen_flag(obj) == GC_YOUNG_GEN) {
    SObject* location;
    if ((char*)obj >= snap->to_space->begin &&
        (char*)obj < snap->to_space->end) {
      return;
    }
    if (!gc_relocated_flag(obj)) {
      size_t size = gc_size(obj);
      location = gc_semi_space_allocate(snap->to_space, obj->type, size);
      if (!location) {
        snap->to_space = gc_semi_space_new(SNAP_SEMI_SPACE_CHUNK_SIZE, snap->to_space);
        location = gc_semi_space_allocate(snap->to_space, obj->type, size);
      }
      memcpy(location, obj, size);
      gc_set_relocated_flag(obj);
      ((SObjectRelocated*)obj)->location = location;
    } else {
      location = ((SObjectRelocated*)obj)->location;
    }
    *ptr = location;
  } else {
    assert(false && "Not implemented");
  }
}

static void gc_mark_val(Snap* snap, SValue* val) {
  if (is_obj_p(val)) gc_mark(snap, &val->o);
}

static void gc_mark_children(Snap* snap, SObject* obj) {
  int i;
  switch (obj->type) {
    case STYPE_ARR:
      for (i = 0; i < ((SArr*)obj)->len; ++i) {
        gc_mark_val(snap, &((SArr*)obj)->data[i]);
      }
      break;
  }
}

static SnapSemiSpace* gc_semi_space_new(size_t size, SnapSemiSpace* next) {
  SnapSemiSpace* space = (SnapSemiSpace*)malloc(sizeof(SnapSemiSpace) + size);
  space->pos = space->begin;
  space->end = space->begin + size;
  space->next = next;
  return space;
}

static SObject* gc_semi_space_allocate(SnapSemiSpace* space, uint8_t type, size_t size) {
  SObject* obj;
  if (space->pos + size > space->end) {
    if (space->next) {
      return gc_semi_space_allocate(space->next, type, size);
    }
    return NULL;
  }
  obj = (SObject*)space->pos;
  space->pos += size;
  obj->type = type;
  obj->flags = 0;
  obj->size = size > UINT16_MAX ? 0 : size;
  return obj;
}

static void gc_semi_space_collect(Snap* snap, SnapSemiSpace* to_space) {
  char* pos = to_space->begin;
  char* end = to_space->pos;
  while (pos < end) {
    SObject* obj = (SObject*) pos;
    gc_mark_children(snap, obj);
    pos += gc_size(obj);
  }
}

static size_t gc_semi_space_allocated(SnapSemiSpace* to_space) {
  size_t size = 0;
  SnapSemiSpace* space = to_space;
  while (space) {
    size = space->pos - space->begin;
    space = space->next;
  }
  return size;
}

static void gc_semi_space_reset(SnapSemiSpace* from_space) {
  SnapSemiSpace* space = from_space;
  while (space) {
    space->pos = space->begin;
    space = space->next;
  }
}

#define SNAP_STACK_BEGIN(...) \
  SnapStack stack__; \
  SObject** objs__[] = { __VA_ARGS__ }; \
  stack__.next = snap->roots; \
  snap->roots = &stack__; \
  stack__.objs = objs__; \
  stack__.count = sizeof(objs__) / sizeof(SObject**)

#define SNAP_OBJECT(obj) ((SObject**)&(obj))

#define SNAP_STACK_END() \
  snap->roots = stack__.next

#define SNAP_RETURN_WITH(value) \
  SNAP_STACK_END(); \
  return (value);

#define SNAP_RETURN() \
  SNAP_STACK_END(); \
  return;

void gc_collect(Snap* snap, bool young_only) {
  SnapStack* root;

  /* Swap the to/from spaces */
  {
    SnapSemiSpace* temp = snap->to_space;
    snap->to_space = snap->from_space;
    snap->from_space = temp;
  }

  if (snap->to_space) {
    gc_semi_space_reset(snap->to_space);
  } else {
    snap->to_space = gc_semi_space_new(SNAP_SEMI_SPACE_CHUNK_SIZE, NULL);
  }

  for (root = snap->roots; root != NULL; root = root->next) {
    size_t i;
    for (i = 0; i < root->count; ++i) {
      gc_mark(snap, root->objs[i]);
    }
  }

  gc_semi_space_collect(snap, snap->to_space);
  snap->young_gen_allocated = gc_semi_space_allocated(snap->to_space);
}

SObject* gc_new(Snap* snap, uint8_t type, size_t size) {
  SObject* obj;
  size = round_to_alignment(size, sizeof(SObjectRelocated));
  if (size <= SNAP_YOUNG_GEN_MAX_OBJECT_SIZE) {
    if (snap->young_gen_allocated > SNAP_YOUNG_GEN_MAX_SIZE) {
      gc_collect(snap, true);
    }
    obj = gc_semi_space_allocate(snap->to_space, type, size);
    if (!obj) {
      snap->to_space = gc_semi_space_new(SNAP_SEMI_SPACE_CHUNK_SIZE, snap->to_space);
      obj = gc_semi_space_allocate(snap->to_space, type, size);
    }
    snap->young_gen_allocated += size;
  } else {
    assert(false && "Not implemented");
  }
  return obj;
}

SSymStr* snap_str_new(Snap* snap, const char* str) {
  size_t len = strlen(str);
  SSymStr* s = (SSymStr*)gc_new(snap, STYPE_STR, sizeof(SSymStr) + len + 1);
  s->len = len;
  strcpy(s->data, str);
  return s;
}

SArr* snap_arr_new(Snap* snap, int len) {
  SArr* a = (SArr*)gc_new(snap, STYPE_ARR, sizeof(SArr) + len * sizeof(SValue));
  a->len = len;
  return a;
}

void snap_init(Snap* snap) {
  snap->to_space = gc_semi_space_new(SNAP_SEMI_SPACE_CHUNK_SIZE, NULL);
  snap->from_space = NULL;
  snap->young_gen_allocated = 0;
  snap->roots = NULL;
}

int snap_dummy1(Snap* snap) {
  SObject* obj1 = NULL;
  SObject* obj2 = NULL;
  SStuff* stuff1 = NULL;

  SNAP_STACK_BEGIN(SNAP_OBJECT(obj1),
                   SNAP_OBJECT(obj1),
                   SNAP_OBJECT(obj2),
                   SNAP_OBJECT(stuff1));

  obj1 = gc_new(snap, 0, sizeof(SObject));
  obj2 = obj1;
  stuff1 = (SStuff*)gc_new(snap, 0, sizeof(SStuff));

  printf("obj1 %p\n", obj1);
  printf("obj2 %p\n", obj2);
  printf("stuff1 %p\n", stuff1);

  gc_collect(snap, true);

#if 0
  {
    SObject* local1 = NULL;

    SNAP_STACK_BEGIN(SNAP_OBJECT(local1));

    printf("local1 %p\n", local1);

    printf("local1 %p\n", local1);

    SNAP_STACK_END();
  }
#endif

  printf("obj1 %p\n", obj1);
  printf("obj2 %p\n", obj2);
  printf("stuff1 %p\n", stuff1);

  SNAP_RETURN_WITH(0);
}

void snap_dummy2(Snap* snap) {
  SObject* obj1 = NULL;
  SObject* obj2 = NULL;

  SNAP_STACK_BEGIN(SNAP_OBJECT(obj1),
                   SNAP_OBJECT(obj2));

  printf("obj1 %p\n", obj1);
  printf("obj2 %p\n", obj2);

  snap_dummy1(snap);

  printf("obj1 %p\n", obj1);
  printf("obj2 %p\n", obj2);

  SNAP_RETURN();
}

void snap_dummy3(Snap* snap) {
  SObject* obj1 = NULL;
  SObject* obj2 = NULL;
  SObject* obj3 = NULL;

  SNAP_STACK_BEGIN(SNAP_OBJECT(obj1),
                   SNAP_OBJECT(obj2),
                   SNAP_OBJECT(obj3));

  printf("obj1 %p\n", obj1);
  printf("obj2 %p\n", obj2);
  printf("obj2 %p\n", obj3);

  snap_dummy2(snap);

  printf("obj1 %p\n", obj1);
  printf("obj2 %p\n", obj2);
  printf("obj2 %p\n", obj3);

  SNAP_RETURN();
}

void snap_many(Snap* snap) {
  int i;
  SArr* arr = NULL;

  SNAP_STACK_BEGIN(SNAP_OBJECT(arr));

  arr = snap_arr_new(snap, 4000);

  for (i = 0; i < 4000; ++i) {
    SSymStr* str = NULL;

    SNAP_STACK_BEGIN(SNAP_OBJECT(str));

    char buf[1024];
    sprintf(buf, "%d "
                 "0123456701234567012345670123456701234567012345670123456701234567"
                 "0123456701234567012345670123456701234567012345670123456701234567", i);
    str = snap_str_new(snap, buf);

    arr->data[i] = create_obj(str);

    SNAP_STACK_END();
  }

  gc_collect(snap, true);

  SNAP_RETURN();
}

int main() {
  //Snap snap;
  //snap_init(&snap);

  //snap_many(&snap);

  //snap_dummy1(&snap);
  //snap_dummy3(&snap);

  printf("%zx\n", (~((size_t)0)) - 1);
}
