#ifndef SNAP_GC_H_H
#define SNAP_GC_H_H

#include <stdint.h>
#include <stddef.h>

enum {
  GC_YOUNG_GEN,
  GC_MATURE_GEN
};

#define GC_OBJECT_FIELDS \
  uint8_t type;          \
  uint8_t flags;         \
  uint16_t size;

typedef struct {
  GC_OBJECT_FIELDS
} GCObject;

typedef struct {
  GC_OBJECT_FIELDS
  GCObject* location;
} GCObjectRelocated;

typedef struct GCStack_ {
  struct GCStack_* next;
  GCObject*** objs;
  size_t count;
} GCStack;

typedef struct GCSemiSpace_ {
  char* pos;
  char* end;
  struct GCSemiSpace_* next;
  char begin[1];
} GCSemiSpace;

typedef struct GC_ GC;

typedef void (*GCMarkChildren)(GC*, GCObject*);
typedef size_t (*GCSlowSize)(GCObject*);

typedef void* (*GCMalloc)(size_t);
typedef void (*GCFree)(void*);

typedef struct GC_ {
  GCSemiSpace* to_space;
  GCSemiSpace* from_space;
  size_t semi_chuck_size;
  size_t semi_max_allocated;
  size_t allocated;
  GCMalloc malloc;
  GCFree free;
  GCMarkChildren mark_children;
  GCSlowSize slow_size;
  GCStack* roots;
} GC;

void gc_init(GC* gc,
             GCMarkChildren mark_children,
             GCSlowSize slow_size);
void gc_destroy(GC* gc);

GCObject* gc_new(GC* gc, uint8_t type, size_t size);
GCObject* gc_new_container(GC* gc, uint8_t type, size_t size);

void gc_mark(GC* gc, GCObject** ptr);
void gc_collect(GC* gc);

#define GC_BEGIN(...) \
  GCStack stack__; \
  GCObject** objs__[] = { __VA_ARGS__ }; \
  stack__.next = (GETGC())->roots; \
  (GETGC())->roots = &stack__; \
  stack__.objs = objs__; \
  stack__.count = sizeof(objs__) / sizeof(GCObject**)

#define GC_OBJ(obj) ((GCObject**)(obj = NULL, &(obj)))

#define GC_END() \
  (GETGC())->roots = stack__.next

#define GC_RETURN_WITH(value) \
  GC_END(); \
  return (value);

#define GC_RETURN() \
  GC_END(); \
  return;

#endif // SNAP_GC_H_H
