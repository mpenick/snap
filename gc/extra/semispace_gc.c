#include <assert.h>

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "gc.h"
#include "mstructs.h"

#define gc_set_relocated_flag(obj) \
  (obj)->flags.mark = 1

#define gc_relocated_flag(obj) \
  ((obj)->flags.mark)

#define gc_set_is_container(obj) \
  (obj)->flags.is_container = 1

#define gc_is_container(obj) \
  ((obj)->flags.is_container)

#define gc_size(obj) ((obj)->size > 0 ? (obj)->size : gc->slow_size(obj))

static inline size_t round_to_alignment(size_t value, size_t alignment) {
   return (value + (alignment - 1)) & ~(alignment - 1);
}

static GCSemiSpace* gc_semi_space_new(GC* gc, GCSemiSpace* next);
static void* gc_semi_space_allocate(GCSemiSpace* space, size_t size);

static GCSemiSpace* gc_semi_space_new(GC* gc, GCSemiSpace* next) {
  GCSemiSpace* space = (GCSemiSpace*)gc->malloc(sizeof(GCSemiSpace) + gc->semi_chuck_size);
  space->pos = (uintptr_t)space->begin;
  space->end = (uintptr_t)space->begin + gc->semi_chuck_size;
  space->next = next;
  return space;
}

static void gc_semi_space_delete(GC* gc, GCSemiSpace* semi) {
  GCSemiSpace* curr = gc->to_space;
  while (curr) {
    GCSemiSpace* temp = curr->next;
    gc->free(curr);
    curr = temp;
  }
}

static void* gc_semi_space_allocate(GCSemiSpace* space, size_t size) {
  void* ptr;
  if (space->pos + size > space->end) {
    if (space->next) {
      return gc_semi_space_allocate(space->next, size);
    }
    return NULL;
  }
  ptr = (void*)space->pos;
  space->pos += size;
  return ptr;
}

static void gc_semi_space_collect(GC* gc, GCSemiSpace* to_space) {
  uintptr_t pos = (uintptr_t)to_space->begin;
  uintptr_t end = to_space->pos;
  while (pos < end) {
    GCObject* obj = (GCObject*)pos;
    if (gc_is_container(obj)) {
      gc->mark_children(gc, obj);
    }
    pos += gc_size(obj);
  }
}

static size_t gc_semi_space_allocated(GCSemiSpace* to_space) {
  size_t size = 0;
  GCSemiSpace* space = to_space;
  while (space) {
    size = (size_t)(space->pos - (uintptr_t)space->begin);
    space = space->next;
  }
  return size;
}

static void gc_semi_space_reset(GCSemiSpace* from_space) {
  GCSemiSpace* space = from_space;
  while (space) {
    space->pos = (uintptr_t)space->begin;
    space = space->next;
  }
}

void gc_mark(GC* gc, GCObject** ptr) {
  void* new_location;
  void* old_location = *ptr;
  if (!old_location) return;
  if (!gc_relocated_flag(*ptr)) {
    size_t size = gc_size(*ptr);
    new_location = gc_semi_space_allocate(gc->to_space, size);
    if (!new_location) {
      gc->to_space = gc_semi_space_new(gc, gc->to_space);
      new_location = gc_semi_space_allocate(gc->to_space, size);
    }
    memcpy(new_location, old_location, size);
    ((GCObjectRelocated*)old_location)->location = old_location;
  } else {
    new_location = ((GCObjectRelocated*)old_location)->location;
  }
  *ptr = new_location;
}

void gc_collect(GC* gc) {
  GCStack* root;

  /* Swap the to/from spaces */
  {
    GCSemiSpace* temp = gc->to_space;
    gc->to_space = gc->from_space;
    gc->from_space = temp;
  }

  if (gc->to_space) {
    gc_semi_space_reset(gc->to_space);
  } else {
    gc->to_space = gc_semi_space_new(gc, NULL);
  }

  for (root = gc->roots; root != NULL; root = root->next) {
    size_t i;
    for (i = 0; i < root->count; ++i) {
      gc_mark(gc, root->objs[i]);
    }
  }

  gc_semi_space_collect(gc, gc->to_space);
  gc->allocated = gc_semi_space_allocated(gc->to_space);
}

void gc_init(GC* gc,
             GCMarkChildren mark_children,
             GCSlowSize slow_size) {
  gc->semi_chuck_size = 256 * 1024; // 256kb
  gc->semi_max_allocated = 2 * 1024 * 1024; // 2mb
  gc->semi_too_large = (gc->semi_chuck_size * 10) / 100; // 10% of chunk size
  gc->malloc = malloc;
  gc->free = free;

  gc->to_space = gc_semi_space_new(gc, NULL);
  gc->from_space = NULL;
  gc->mature_all = NULL;
  gc->mature_gray = NULL;
  gc->allocated = 0;
  gc->mark_children = mark_children;
  gc->slow_size = slow_size;
  gc->roots = NULL;
}

void gc_destroy(GC* gc) {
  // TODO
  gc_semi_space_delete(gc, gc->to_space);
  gc_semi_space_delete(gc, gc->from_space);
}

void gc_obj_init(GCObject* obj, uint8_t type, GCGen gen, size_t size) {
  obj->type = type;
  obj->flags = (GCFlags) { 0, 0, (uint8_t)gen };
  obj->size = size > UINT16_MAX ? 0 : (uint16_t)size;
}

GCObject* gc_new(GC* gc, uint8_t type, size_t size) {
  GCObject* obj;
  GCGen gen;
  size_t aligned_size = round_to_alignment(size, sizeof(GCObjectRelocated));
  if (aligned_size < gc->semi_too_large) {
    size = aligned_size;
    if (gc->allocated > gc->semi_max_allocated) {
      gc_collect(gc);
    }
    obj = gc_semi_space_allocate(gc->to_space, size);
    if (!obj) {
      gc->to_space = gc_semi_space_new(gc, gc->to_space);
      obj = gc_semi_space_allocate(gc->to_space, size);
    }
    gen = GC_YOUNG_GEN;
  } else {
    GCObjectMature* mature = gc->malloc((sizeof(GCObjectMature) - offsetof(GCObjectMature, obj)) + size);
    mature->next = gc->mature_all;
    gc->mature_all = mature;
    mature->gray_next = NULL;
    obj = &mature->obj.gc_obj;
    gen = GC_MATURE_GEN;
  }
  gc_obj_init(obj, type, gen, size);
  gc->allocated += size;
  return obj;
}

GCObject* gc_new_container(GC* gc, uint8_t type, size_t size) {
  GCObject* obj = gc_new(gc, type, size);
  gc_set_is_container(obj);
  return obj;
}

#if 0
// Marked == Moved

#define PTRMASK (~((size_t)0)) - 1)
#define RB(obj) (obj & 0x1) ?

typedef struct GCObject SObject;

typedef struct Snap_ Snap;

typedef struct SCode_ SCode;
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
    GCObject* o;
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

typedef struct SSymStr_ {
  GC_OBJECT_FIELDS
  size_t len;
  char data[0];
} SSymStr;

typedef struct SKeyword_ {
  GC_OBJECT_FIELDS
  int id;
  size_t len;
  char data[0];
} SKeyword;

typedef struct SArr_ {
  GC_OBJECT_FIELDS
  int len;
  SValue data[0];
} SArr;

typedef struct SStuff_ {
  GC_OBJECT_FIELDS
  int stuff;
} SStuff;

SValue create_obj_(GCObject* o) {
  SValue val;
  val.type = o->type;
  val.o = o;
  return val;
}

#define create_obj(o) create_obj_((SObject*)o)

struct Snap_ {
  GC gc;
};

SSymStr* snap_str_new(Snap* snap, const char* str) {
  size_t len = strlen(str);
  SSymStr* s = (SSymStr*)gc_new(snap, STYPE_STR, sizeof(SSymStr) + len + 1);
  s->len = len;
  strcpy(s->data, str);
  return s;
}

SArr* snap_arr_new(Snap* snap, int len) {
  SArr* a = (SArr*)gc_new_container(snap, STYPE_ARR, sizeof(SArr) + len * sizeof(SValue));
  a->len = len;
  return a;
}

static void snap_gc_mark_val(GC* gc, SValue* val) {
  if (is_obj_p(val)) gc_mark(gc, &val->o);
}

static void snap_gc_mark_children(GC* gc, GCObject* obj) {
  int i;
  switch (obj->type) {
    case STYPE_ARR:
      for (i = 0; i < ((SArr*)obj)->len; ++i) {
        snap_gc_mark_val(gc, &((SArr*)obj)->data[i]);
      }
      break;
  }
}

void snap_init(Snap* snap) {
  gc_init(&snap->gc, snap_gc_mark_children, NULL);
}

void snap_destroy(Snap* snap) {
  gc_destroy(&snap->gc);
}

#define GETGC() (&snap->gc)

int snap_dummy1(Snap* snap) {
  GCObject* obj1 = NULL;
  GCObject* obj2 = NULL;
  SStuff* stuff1 = NULL;

  GC_BEGIN(GC_OBJ(obj1),
           GC_OBJ(obj1),
           GC_OBJ(obj2),
           GC_OBJ(stuff1));

  obj1 = gc_new(snap, 0, sizeof(GCObject));
  obj2 = obj1;
  stuff1 = (SStuff*)gc_new(snap, 0, sizeof(SStuff));

  printf("obj1 %p\n", obj1);
  printf("obj2 %p\n", obj2);
  printf("stuff1 %p\n", stuff1);

  gc_collect(snap);

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

  GC_RETURN_WITH(0);
}

void snap_dummy2(Snap* snap) {
  GCObject* obj1 = NULL;
  GCObject* obj2 = NULL;

  GC_BEGIN(GC_OBJ(obj1),
           GC_OBJ(obj2));

  printf("obj1 %p\n", obj1);
  printf("obj2 %p\n", obj2);

  snap_dummy1(snap);

  printf("obj1 %p\n", obj1);
  printf("obj2 %p\n", obj2);

  GC_RETURN();
}

void snap_dummy3(Snap* snap) {
  GCObject* obj1 = NULL;
  GCObject* obj2 = NULL;
  GCObject* obj3 = NULL;

  GC_BEGIN(GC_OBJ(obj1),
                   GC_OBJ(obj2),
                   GC_OBJ(obj3));

  printf("obj1 %p\n", obj1);
  printf("obj2 %p\n", obj2);
  printf("obj2 %p\n", obj3);

  snap_dummy2(snap);

  printf("obj1 %p\n", obj1);
  printf("obj2 %p\n", obj2);
  printf("obj2 %p\n", obj3);

  GC_RETURN();
}

void snap_many(Snap* snap) {
  int i;
  SArr* arr = NULL;

  GC_BEGIN(GC_OBJ(arr));

  arr = snap_arr_new(snap, 4000);

  for (i = 0; i < 4000; ++i) {
    SSymStr* str;

    GC_BEGIN(GC_OBJ(str));

    char buf[1024];
    sprintf(buf, "%d "
                 "0123456701234567012345670123456701234567012345670123456701234567"
                 "0123456701234567012345670123456701234567012345670123456701234567"
                 "0123456701234567012345670123456701234567012345670123456701234567"
                 "0123456701234567012345670123456701234567012345670123456701234567"
                 "0123456701234567012345670123456701234567012345670123456701234567"
                 "0123456701234567012345670123456701234567012345670123456701234567"
                 "0123456701234567012345670123456701234567012345670123456701234567"
                 "0123456701234567012345670123456701234567012345670123456701234567"
                 "0123456701234567012345670123456701234567012345670123456701234567"
                 "0123456701234567012345670123456701234567012345670123456701234567", i);
    str = snap_str_new(snap, buf);

    arr->data[i] = create_obj(str);

    GC_END();
  }

  gc_collect(&snap->gc);

  GC_RETURN();
}
#endif

typedef struct {
  GCObject gc_obj;
  size_t len;
  char data[1];
} SSymStr;

GC_MATURE_DECL(SSymStr);

int main() {
  //Snap snap;
  //snap_init(&snap);

  //snap_many(&snap);

  //snap_destroy(&snap);

  //snap_dummy1(&snap);
  //snap_dummy3(&snap);

  //printf("%zx\n", (~((size_t)0)) - 1);
  //GCObjectMature m;
  //printf("sizeof(SArr) = %zu\n", sizeof(SArr));
  //printf("offsetof(SArr, type) = %zu\n", offsetof(SArr, type));
  //printf("offsetof(SArr, len) = %zu\n", offsetof(SArr, len));

  printf("sizeof(GCObject) = %zu\n", sizeof(GCObject));
  printf("sizeof(GCObjectRelocated) = %zu\n", sizeof(GCObjectRelocated));
  printf("sizeof(SSymStr) = %zu\n", sizeof(SSymStr));
  printf("sizeof(SSymStrMature) = %zu\n", sizeof(GC_MATURE(SSymStr)));

  //printf("sizeof(GCObjectMature) = %zu\n", sizeof(GCObjectMature));
  //printf("offsetof(GCObjectMature, next) = %zu\n", offsetof(GCObjectMature, next));
  //printf("offsetof(GCObjectMature, obj) = %zu\n", offsetof(GCObjectMature, obj));
}
