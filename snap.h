#ifndef SNAP_H
#define SNAP_H

#include "mstructs.h"

#include <assert.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#define check(a, e) (assert(a), (e))

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
  XX(STYPE_KEY, 10, "key") \
  XX(STYPE_ERR, 11, "err") \
  XX(STYPE_CONS, 12, "cons") \
  XX(STYPE_VEC, 13, "vector") \
  XX(STYPE_MAP, 14, "map") \
  XX(STYPE_CODE, 15, "code") \
  XX(STYPE_CLOSURE, 16, "closure") \
  XX(STYPE_UPVAL, 17, "upval") \
  XX(STYPE_TEMPSTR, 18, "tempstr") \
  XX(STYPE_TYPE, 19, "type") \
  XX(STYPE_STRUCT, 20, "struct") \
  XX(STYPE_METHOD, 21, "method")

enum {
#define XX(type, id, name) type,
  STYPE_MAPPING(XX)
#undef XX
};

typedef struct Snap_ Snap;
typedef struct SCode_ SCode;
typedef struct SObject_ SObject;
typedef struct SValue_ SValue;

typedef int64_t SnapInt;
typedef double SnapFloat;
// Returns false to panic, with *result set to the error
typedef bool (*SCFunc)(Snap* snap, const SValue* args, int num_args, SValue* result);

// Opcode in the low 8 bits, signed argument in the high 24 bits
typedef uint32_t SnapInst;

// A value is one tagged word:
//   ...xxx1  63-bit integer (i << 1 | 1)
//   ...x000  pointer to a GC object, whose header holds its type
//   ...x010  undef, deleted, nil, false, true, the empty list (n << 3 | 2) or
//            a special form ((SV_FORMS + token) << 3 | 2)
//   ...x100  float, unless it's too big or small (see snap_imm_float())
//   ...x110  C function (pointer << 3 | 6; user space pointers fit in 61 bits)
struct SValue_ {
  uint64_t bits;
};

#define TAG_MASK 7
#define TAG_OBJ 0
#define TAG_SPECIAL 2
#define TAG_FLOAT 4
#define TAG_CFUNC 6

#define SV_SPECIAL(n) (((uint64_t)(n) << 3) | TAG_SPECIAL)
#define SV_UNDEF SV_SPECIAL(0)
#define SV_DELETED SV_SPECIAL(1)
#define SV_NIL SV_SPECIAL(2)
#define SV_FALSE SV_SPECIAL(3)
#define SV_TRUE SV_SPECIAL(4)
static_assert((SV_NIL | 8) == SV_FALSE, "is_falsy() needs nil and false to differ only in bit 3");
#define SV_EMPTY SV_SPECIAL(5)
#define SV_FORMS 8 // Special forms come after the specials

// Floats with exponents from -126 to 128 (magnitudes from about 1e-38 to 6e38)
// and zeros are immediate, as in Spur (Smalltalk): the bits are rotated so the
// sign is lowest, and the 11-bit exponent is offset to fit in 8 bits, leaving
// room for the tag. The mantissa is kept, so they're exact. Other floats
// (including infinities and NaN) are boxed in objects.
#define FLOAT_EXP_OFFSET ((uint64_t)(1023 - 127) << 53)

#define SNAP_INT_MAX ((SnapInt)(((uint64_t)1 << 62) - 1))
#define SNAP_INT_MIN (-SNAP_INT_MAX - 1)

int snap_hash(const SValue* val);
int snap_compare(const SValue* val1, const SValue* val2);

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

#define SOBJECT_FIELDS          \
  uint8_t type;                 \
  uint8_t mark;                 \
  uint8_t read_only; /* Vectors and maps from quote */ \
  uint8_t macro;     /* Functions from defmacro */

struct SObject_ {
  SOBJECT_FIELDS
};

typedef struct SFloat_ {
  SOBJECT_FIELDS
  SnapFloat f;
} SFloat;

typedef struct SStr_ {
  SOBJECT_FIELDS
  size_t len;
  char data[1];
} SStr;

// Symbols are interned and hold their global binding (undef when unbound)
typedef struct SSym_ {
  SOBJECT_FIELDS
  SValue value;
  size_t len;
  char data[1];
} SSym;

typedef struct SKeyword_ {
  SOBJECT_FIELDS
  int id;
  size_t len;
  char data[1];
} SKeyword;

typedef struct STempStr_ {
  SOBJECT_FIELDS
  size_t len;
  const char* data;
} STempStr;

typedef struct SErr_ {
  SOBJECT_FIELDS
  SValue err;
  SValue msg;
} SErr;

typedef struct SCons_ {
  SOBJECT_FIELDS
  SValue first;
  SValue rest;
} SCons;

// Mutable and growable
// A vector's values are in the array it was made with, then in segments that
// double in size (8, 16, 32...) as it grows, so growing never copies them
#define SNAP_VEC_SEG_BITS 3
#define SNAP_VEC_SEG (1 << SNAP_VEC_SEG_BITS)

typedef struct SVec_ {
  SOBJECT_FIELDS
  uint8_t num_segs;
  int len;
  int cap0;       // Size of data
  SValue* data;
  SValue** segs;  // Segment k holds SNAP_VEC_SEG << k values
} SVec;

#define snap_vec_cap(v) ((v)->cap0 + (SNAP_VEC_SEG << (v)->num_segs) - SNAP_VEC_SEG)

// Mutable hash map that keeps insertion order. Entries are key/value pairs in
// the order they were added; a deleted entry's key is SV_DELETED until the
// next rebuild. The index maps hash slots to entries (-1 when empty) and is
// kept at most half full, deleted entries included.
typedef struct SMap_ {
  SOBJECT_FIELDS
  int len;  // Live entries
  int used; // Entries, including deleted ones
  int cap;  // Entry capacity, index_cap / 2
  SValue* entries;
  int32_t* index;
} SMap;

// A type, from defstruct or built in (Int, Vector...). Types are never freed.
// Calling a struct type makes an instance, with values for its fields in order.
typedef struct SType_ {
  SOBJECT_FIELDS
  bool is_struct;
  int num_fields;
  SSym* name;
  SValue gc; // Finalizer for its structs, or nil (see snap_set_gc()). Pinned, as types aren't scanned.
  SValue fields[0]; // Keywords
} SType;

typedef struct SStruct_ {
  SOBJECT_FIELDS
  uint32_t num_vals; // Its type's num_fields, for freeing it after its type in snap_destroy()
  SType* stype;
  SValue vals[0];
} SStruct;

// A function with an implementation for each type, picked by the type of its
// first argument (see defmethod)
typedef struct SMethod_ {
  SOBJECT_FIELDS
  SSym* name;
  SMap* table; // Type to function
} SMethod;

typedef struct SUpval_ {
  SOBJECT_FIELDS
  SValue* value; // Points into the stack while open, at `closed` once closed
  SValue closed;
  struct SUpval_* next; // Next open upvalue (sorted by descending stack address)
} SUpval;

typedef struct SClosure_ {
  SOBJECT_FIELDS
  SCode* code;
  int num_upvals;
  SUpval* upvals[0];
} SClosure;

struct SCode_ {
  SOBJECT_FIELDS
  SnapInst* insts;
  SValue* constants;
  uint32_t* upvals; // (index << 1) | islocal, islocal means a slot in the enclosing frame
  int insts_count;
  int num_constants;
  int num_upvals;
  int num_args;
  int arity; // num_args, or -1 if the last parameter takes the rest (a list)
  int num_locals;
  int max_stack_size;
};

typedef struct SnapFrame_ {
  SnapInst* pc;
  SCode* code;
  SClosure* closure;
  SValue* stack_base;
  SValue* stack_top;
  bool recover; // Called by recover: a panic unwinds to here
  bool stop;    // Called from C: exec() returns when it returns or panics
} SnapFrame;

// The most values the stack can hold (8 MB), as in Lua. Calls past it are a
// stack overflow.
#ifndef SNAP_MAX_STACK
#define SNAP_MAX_STACK 1000000
#endif

// How deep C can recurse: through calls of snap code from C (a macro that
// compiles code that uses a macro...), and nesting in the reader and compiler.
// Past it is an error rather than a C stack overflow.
#ifndef SNAP_MAX_C_DEPTH
#define SNAP_MAX_C_DEPTH 1000
#endif

// Objects live in pages of slots of one size, or a page of their own if
// they're big (see snap.c)
#define SNAP_NUM_SLOT_SIZES 8

typedef struct SnapPage_ {
  struct SnapPage_* next;
  uint32_t slot_size;
  uint32_t num_slots;
} SnapPage;

// A marked object and the index of its next child to mark, so that big
// vectors and maps are marked a chunk at a time
typedef struct {
  SObject* obj;
  int pos;
} SnapGray;

// How many of the latest pauses' times are kept, for the median and percentiles
#define SNAP_GC_RECENT 64

// What the collector has done so far (see (gc-stats))
typedef struct {
  size_t collections;  // Finished collections
  size_t steps;        // Pauses: collections started and steps of them
  uint64_t allocated;  // Bytes allocated for objects and what they hold
  double pause_total;  // Seconds spent collecting
  double pause_max;    // The longest pause, in seconds
  double recent[SNAP_GC_RECENT]; // The latest pauses' seconds, at steps % SNAP_GC_RECENT
} SnapGCStats;

struct Snap_ {
  SnapHash symbols;
  SnapHash keywords;
  SValue* stack;
  SValue* stack_end;
  int stack_size;
  struct { MVEC_FIELDS(SnapFrame); } frames;
  bool reentered; // Set by snap_call(), which can move the stack and the frames
  int c_depth; // Counted up to SNAP_MAX_C_DEPTH
  char* load_file; // The file snap_load() is running, which relative paths are from
  uint64_t rand_state; // (rand), set by (rand-seed n)
  struct { MVEC_FIELDS(SObject*); } anchors;
  struct { MVEC_FIELDS(SValue); } pins; // See snap_pin()
  size_t num_bytes_alloced;
  size_t next_gc; // Start a collection when num_bytes_alloced reaches this
  SnapPage* pages;
  SObject* free_slots[SNAP_NUM_SLOT_SIZES]; // A free list for each slot size
  // The collector runs in small steps between allocations (see snap.c)
  uint8_t gc_phase;     // Idle, marking or sweeping
  uint8_t gc_unmarked;  // Mark value of unmarked objects (0 or 1), flipped each collection
  bool gc_manual;       // Only collect in snap_gc_step(), never while allocating
  int gc_sym;           // Next symbol table entry to mark the value of (globals)
  size_t gc_debt;       // Bytes allocated since the last step
  size_t gc_live;       // Estimate of live bytes: the heap when the collection started, less what it freed
  SnapPage** gc_sweep;  // Page being swept
  uint32_t gc_sweep_slot; // Next slot to sweep in it
  struct { MVEC_FIELDS(SnapGray); } gray; // Marked objects whose children still need marking
  // Finalizers (see snap_set_gc()). Structs whose type had one when they were
  // made are in finobj, which isn't a root. When marking ends, the unmarked
  // ones move to tobefnz, which is, until their finalizers run.
  struct { MVEC_FIELDS(SObject*); } finobj;
  struct { MVEC_FIELDS(SValue); } tobefnz;
  bool gc_separated;   // This collection has moved finobj's unmarked structs to tobefnz
  bool fin_pending;    // tobefnz has structs to finalize
  bool fin_running;    // In snap_run_finalizers(), which doesn't nest
  bool gc_closing;     // In snap_destroy(), where new structs don't get finalizers
  SnapGCStats gc_stats;
  SUpval* open_upvals;
  struct SnapFnState_* fn_state; // Innermost function being compiled
  SErr* cause;
  jmp_buf jmp;
  int num_gensyms;
  SType* types[STYPE_METHOD + 1]; // The built-in type of each kind of value
  bool dump_code; // Print the bytecode from snap_compile() and snap_exec()
};

#define is_int(v) ((v).bits & 1)
#define is_obj(v) (((v).bits & TAG_MASK) == TAG_OBJ)
#define is_undef(v) ((v).bits == SV_UNDEF)
#define is_nil(v) ((v).bits == SV_NIL)
#define is_false(v) ((v).bits == SV_FALSE)
// nil and false are false in conditions. They differ only in bit 3.
#define is_falsy(v) (((v).bits & ~(uint64_t)8) == SV_NIL)
#define is_bool(v) (is_false(v) || (v).bits == SV_TRUE)
#define is_empty(v) ((v).bits == SV_EMPTY)
#define is_form(v) (((v).bits & TAG_MASK) == TAG_SPECIAL && (v).bits >= SV_SPECIAL(SV_FORMS))
#define is_cfunc(v) (((v).bits & TAG_MASK) == TAG_CFUNC)
#define is_obj_type(v, t) (is_obj(v) && as_obj(v)->type == (t))
#define is_imm_float(v) (((v).bits & TAG_MASK) == TAG_FLOAT)
#define is_float(v) (is_imm_float(v) || is_obj_type(v, STYPE_FLOAT))
#define is_sym(v) is_obj_type(v, STYPE_SYM)
#define is_str(v) is_obj_type(v, STYPE_STR)
#define is_key(v) is_obj_type(v, STYPE_KEY)
#define is_err(v) is_obj_type(v, STYPE_ERR)
#define is_vec(v) is_obj_type(v, STYPE_VEC)
#define is_map(v) is_obj_type(v, STYPE_MAP)
#define is_code(v) is_obj_type(v, STYPE_CODE)
#define is_closure(v) is_obj_type(v, STYPE_CLOSURE)
#define is_type(v) is_obj_type(v, STYPE_TYPE)
#define is_struct(v) is_obj_type(v, STYPE_STRUCT)
#define is_method(v) is_obj_type(v, STYPE_METHOD)
#define is_cons(v) (is_empty(v) || is_obj_type(v, STYPE_CONS))

#define as_obj(v) ((SObject*)(uintptr_t)(v).bits)
#define as_int(v) check(is_int(v), (SnapInt)(v).bits >> 1)
#define as_float(v) check(is_float(v), is_imm_float(v) ? snap_imm_float_val(v) : ((SFloat*)as_obj(v))->f)
#define as_form(v) check(is_form(v), (int)((v).bits >> 3) - SV_FORMS)
#define as_cfunc(v) check(is_cfunc(v), (SCFunc)(uintptr_t)((v).bits >> 3))
#define as_str(v) check(is_str(v), (SStr*)as_obj(v))
#define as_sym(v) check(is_sym(v), (SSym*)as_obj(v))
#define as_key(v) check(is_key(v), (SKeyword*)as_obj(v))
#define as_err(v) check(is_err(v), (SErr*)as_obj(v))
#define as_vec(v) check(is_vec(v), (SVec*)as_obj(v))
#define as_map(v) check(is_map(v), (SMap*)as_obj(v))
#define as_code(v) check(is_code(v), (SCode*)as_obj(v))
#define as_closure(v) check(is_closure(v), (SClosure*)as_obj(v))
#define as_type(v) check(is_type(v), (SType*)as_obj(v))
#define as_struct(v) check(is_struct(v), (SStruct*)as_obj(v))
#define as_method(v) check(is_method(v), (SMethod*)as_obj(v))
#define as_cons(v) check(is_cons(v), is_empty(v) ? NULL : (SCons*)as_obj(v)) // NULL for ()

static inline SValue snap_val(uint64_t bits) {
  SValue val;
  val.bits = bits;
  return val;
}

static inline SValue create_undef(void) { return snap_val(SV_UNDEF); }
static inline SValue create_nil(void) { return snap_val(SV_NIL); }
static inline SValue create_empty(void) { return snap_val(SV_EMPTY); }
static inline SValue create_bool(bool b) { return snap_val(b ? SV_TRUE : SV_FALSE); }
static inline SValue create_int(SnapInt i) { return snap_val(((uint64_t)i << 1) | 1); }
static inline SValue create_form(int token) { return snap_val(SV_SPECIAL(SV_FORMS + token)); }
static inline SValue create_cfunc(SCFunc c) { return snap_val(((uint64_t)(uintptr_t)c << 3) | TAG_CFUNC); }
static inline SValue create_object(SObject* o) { return snap_val((uint64_t)(uintptr_t)o); }
#define create_obj(o) create_object((SObject*)(o))
SValue create_float(Snap* snap, SnapFloat f); // Allocates if f can't be immediate

// Sets *val to f and returns true if f can be immediate
static inline bool snap_imm_float(SnapFloat f, SValue* val) {
  uint64_t bits, r;
  memcpy(&bits, &f, sizeof(bits));
  r = (bits << 1) | (bits >> 63); // Exponent, mantissa, sign
  if (r - (FLOAT_EXP_OFFSET + ((uint64_t)1 << 53)) < ((uint64_t)255 << 53)) {
    val->bits = ((r - FLOAT_EXP_OFFSET) << 3) | TAG_FLOAT; // Offset exponent from 1 to 255
    return true;
  }
  if (r <= 1) {
    val->bits = (r << 3) | TAG_FLOAT; // Zeros get offset exponent 0
    return true;
  }
  return false;
}

static inline SnapFloat snap_imm_float_val(SValue val) {
  uint64_t r = val.bits >> 3, bits;
  SnapFloat f;
  if (r > 1) r += FLOAT_EXP_OFFSET;
  bits = (r >> 1) | (r << 63);
  memcpy(&f, &bits, sizeof(f));
  return f;
}

static inline int snap_type(SValue val) {
  static const uint8_t specials[] = {
    STYPE_UNDEF, STYPE_DELETED, STYPE_NIL, STYPE_BOOL, STYPE_BOOL, STYPE_CONS
  };
  if (is_int(val)) return STYPE_INT;
  switch (val.bits & TAG_MASK) {
    case TAG_OBJ: return as_obj(val)->type;
    case TAG_SPECIAL: return val.bits < SV_SPECIAL(SV_FORMS) ? specials[val.bits >> 3] : STYPE_FORM;
    case TAG_FLOAT: return STYPE_FLOAT;
    default: return STYPE_CFUNC;
  }
}

SStr* snap_str_new(Snap* snap, const char* str);
SSym* snap_sym_new(Snap* snap, const char* name);
SErr* snap_err_new(Snap* snap, SValue err, SValue msg);
SErr* snap_err_new_format(Snap* snap, const char* err, const char* format, ...);
SCons* snap_cons_new(Snap* snap);
SVec* snap_vec_new(Snap* snap, int cap);
void snap_vec_push(Snap* snap, SVec* vec, SValue val);

static inline SValue* snap_vec_at(SVec* vec, int i) {
  uint64_t t;
  int k;
  if (i < vec->cap0) return &vec->data[i];
  t = (uint64_t)(i - vec->cap0) + SNAP_VEC_SEG; // Segment k starts at t = SNAP_VEC_SEG << k
  k = 63 - __builtin_clzll(t) - SNAP_VEC_SEG_BITS;
  return &vec->segs[k][t - ((uint64_t)SNAP_VEC_SEG << k)];
}
SMap* snap_map_new(Snap* snap);
SType* snap_type_of(Snap* snap, SValue val);
SValue* snap_map_get(SMap* map, SValue key);
void snap_map_put(Snap* snap, SMap* map, SValue key, SValue val);
bool snap_map_del(Snap* snap, SMap* map, SValue key);

SObject* snap_anchor(Snap* snap, SObject* obj);
void snap_release(Snap* snap);
// Keeps val alive while C holds it, between calls, until snap_unpin().
// Unlike anchors, pins can be unpinned in any order and errors don't undo
// them. Pinning a value twice takes two unpins. Returns val.
SValue snap_pin(Snap* snap, SValue val);
void snap_unpin(Snap* snap, SValue val);

void snap_def(Snap* snap, const char* name, SValue val);
void snap_def_cfunc(Snap* snap, const char* name, SCFunc cfunc);
// Compiles the forms in str into a function without arguments. Returns an
// error value if they don't parse or compile, or nil if there aren't any.
SValue snap_compile(Snap* snap, const char* str);
// Runs a function from snap_compile()
SValue snap_run(Snap* snap, SValue fn);
// Calls a function. Returns an error value if it panics. The arguments must be
// rooted.
SValue snap_call(Snap* snap, SValue fn, const SValue* args, int num_args);
// Compiles and runs the forms in str one at a time. Returns the last one's
// value, or the first error (one that doesn't compile, or a panic or error
// value from running one).
SValue snap_exec(Snap* snap, const char* str);
// Runs a file with snap_exec(). A relative path is from the directory of the
// file being loaded, if any (so files can load their neighbors), else the
// current directory. Returns an error value if the file can't be read. An
// error from a file loaded by another file has the file's path in front of
// its message.
SValue snap_load(Snap* snap, const char* path);
void snap_print(SValue value);

// Does about `work` units of collection (one is marking a value or sweeping an
// object), starting a collection if none is running. Returns true when a
// collection finishes. With gc_manual set, this is the only way the collector
// runs, e.g. in a program's idle time each frame.
bool snap_gc_step(Snap* snap, size_t work);

// Gives a struct type a finalizer: fn (a snap or C function, or nil for none)
// is called with each struct of the type made from now on, once nothing can
// reach it, to free what it holds (files, textures...). Like Lua's __gc.
void snap_set_gc(Snap* snap, SType* type, SValue fn);
// Runs the finalizers of the structs the collector has found unreachable,
// newest first. They run snap code, so like snap_call() this can move the
// stack. The VM runs them by itself after C function calls, and snap_exec()
// between forms; this is for C programs that want them run sooner.
void snap_run_finalizers(Snap* snap);

void snap_init(Snap* snap);
void snap_destroy(Snap* snap);

#endif
