#ifndef SNAP_GC_H_H
#define SNAP_GC_H_H

#include <stdint.h>
#include <stddef.h>

typedef enum {
  GC_YOUNG_GEN = 0,
  GC_MATURE_GEN = 0xF
} GCGen;

typedef enum {
  GC_WHITE,
  GC_GRAY,
  GC_BLACK
} GCColor;

typedef struct {
  uint8_t mark : 2;
  uint8_t is_container : 1;
  uint8_t gen : 4;
  uint8_t reserved : 1;
} GCFlags;

typedef struct {
  uint8_t type;
  GCFlags flags;
  uint16_t size;
} GCObject;

typedef struct {
  GCObject gc_obj;
  void* location;
} GCObjectRelocated;

typedef struct GCObjectMature_ {
  struct GCObjectMature_* next;
  struct GCObjectMature_* gray_next;
  GCObjectRelocated obj;
} GCObjectMature;

typedef struct GCStack_ {
  struct GCStack_* next;
  GCObject*** objs;
  size_t count;
} GCStack;

typedef struct GCSemiSpace_ {
  uintptr_t pos;
  uintptr_t end;
  struct GCSemiSpace_* next;
  GCObjectRelocated begin[1];
} GCSemiSpace;

typedef struct GC_ GC;

typedef void (*GCMarkChildren)(GC*, GCObject*);
typedef size_t (*GCSlowSize)(GCObject*);

typedef void* (*GCMalloc)(size_t);
typedef void (*GCFree)(void*);

typedef struct GC_ {
  GCSemiSpace* to_space;
  GCSemiSpace* from_space;
  GCObjectMature* mature_all;
  GCObjectMature* mature_gray;
  size_t semi_chuck_size;
  size_t semi_max_allocated;
  size_t semi_too_large;
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

#define gc_is_mature(obj) \
  ((obj)->flags.gen == GC_MATURE_GEN)

#define gc_is_young(obj) \
  ((obj)->flags.gen < GC_MATURE_GEN)

#define gc_is_gray(obj) \
  ((obj)->flags.mark == GC_GRAY)

inline void gc_barrier(GC* gc, GCObject* obj, GCObject* container) {
  if (gc_is_young(obj) &&
      gc_is_mature(container) && !gc_is_gray(container)) {
    // TODO: Calculate GCObjectMature
    //void* ptr = container;
    //((GCObjectMature*)ptr)->gray_next = gc->mature_gray;
    //gc->mature_gray = ((GCObjectMature*)ptr);
  }
}

#define GC_MATURE(type) type##Mature

#define GC_MATURE_DECL(type) \
typedef struct {               \
  GCObjectMature gc_mature;    \
  type obj;                    \
} GC_MATURE(type)              \

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
