// The bytecode and the interpreter (snap_vm.c): opcodes, their encoding and
// the number ops the VM, the compiler and the builtins share.
#ifndef SNAP_SNAP_VM_H
#define SNAP_SNAP_VM_H

#include "snap.h"
#include <math.h>

#define MAX_ARG ((1 << 23) - 1)

#define OPCODE_MAP(XX) \
  XX(LOAD_GLOBAL) \
  XX(STORE_GLOBAL) \
  XX(LOAD_LOCAL) \
  XX(STORE_LOCAL) \
  XX(LOAD_CLOSED) \
  XX(STORE_CLOSED) \
  XX(LOAD_CONSTANT) \
  XX(LOAD_NIL) \
  XX(POP) \
  XX(DUP) \
  XX(ROT2) \
  XX(ROT3) \
  XX(CALL) \
  XX(CALL_GLOBAL) \
  XX(NTH) \
  XX(COUNT) \
  XX(GET) \
  XX(PUT) \
  XX(INT) \
  XX(MIN) \
  XX(MAX) \
  XX(CLOSURE) \
  XX(CLOSE_UPVALS) \
  XX(RETURN) \
  XX(RETURN_VEC) \
  XX(UNPACK) \
  XX(JUMP) \
  XX(JUMP_FALSE) \
  XX(JUMP_TRUE) \
  XX(JUMP_FALSE_OR_POP) \
  XX(JUMP_TRUE_OR_POP) \
  XX(TRY) \
  XX(RECOVER) \
  XX(PANIC) \
  XX(ADD) \
  XX(SUB) \
  XX(MUL) \
  XX(DIV) \
  XX(MOD) \
  XX(LESS_THAN) \
  XX(LESS_EQUALS) \
  XX(GREATER_THAN) \
  XX(GREATER_EQUALS) \
  XX(EQUALS) \
  XX(NOT) \
  XX(STEP_LT) \
  XX(STEP_LE) \
  XX(STEP_GT) \
  XX(STEP_GE) \
  BINARY_VARIANTS(XX, ADD) \
  BINARY_VARIANTS(XX, SUB) \
  BINARY_VARIANTS(XX, MUL) \
  BINARY_VARIANTS(XX, DIV) \
  BINARY_VARIANTS(XX, MOD) \
  BINARY_VARIANTS(XX, LESS_THAN) \
  BINARY_VARIANTS(XX, LESS_EQUALS) \
  BINARY_VARIANTS(XX, GREATER_THAN) \
  BINARY_VARIANTS(XX, GREATER_EQUALS) \
  BINARY_VARIANTS(XX, EQUALS)

// Each binary op has a variant for every kind of left and right operand:
// S (stack), L (local) or K (constant), in this order. They have the same
// encoding; specialize() picks them once a function is compiled.
#define BINARY_VARIANTS(XX, op) \
  XX(op##_SS) XX(op##_SL) XX(op##_SK) \
  XX(op##_LS) XX(op##_LL) XX(op##_LK) \
  XX(op##_KS) XX(op##_KL) XX(op##_KK)

enum {
#define XX(opcode) opcode,
  OPCODE_MAP(XX)
#undef XX
};

#define INST(op, arg) (((uint32_t)(arg) << 8) | (uint32_t)(op))
#define INST_OP(inst) ((inst) & 0xFF)
#define INST_ARG(inst) ((int32_t)(inst) >> 8)

// Binary ops (ADD..EQUALS) can take their operands directly from a local or a
// constant, and arithmetic ops can store their result in a local. The arg
// holds an unsigned 8-bit destination then two signed 8-bit operands:
//   | dest:8 | left:8 | right:8 | op:8 |
// An operand of 0 is the stack, n > 0 is local n - 1 and n < 0 is constant
// -n - 1. A destination of 0 pushes and n > 0 stores in local n - 1.
#define is_binary_op(op) ((op) >= ADD && (op) <= EQUALS)
#define is_arith_op(op) ((op) >= ADD && (op) <= MOD)
#define OPERAND_DEST(inst) ((inst) >> 24)
#define OPERAND_LEFT(inst) ((int8_t)((inst) >> 16))
#define OPERAND_RIGHT(inst) ((int8_t)((inst) >> 8))
#define OPERANDS(dest, left, right) \
  ((int)(((uint32_t)(dest) << 16) | (((uint32_t)(left) & 0xFF) << 8) | ((uint32_t)(right) & 0xFF)))
#define MAX_OPERAND 127
#define MAX_DEST 255

#define operand_kind(n) ((n) == 0 ? 0 : (n) > 0 ? 1 : 2) // S, L or K
#define variant(op, inst) \
  (ADD_SS + ((op) - ADD) * 9 + operand_kind(OPERAND_LEFT(inst)) * 3 + operand_kind(OPERAND_RIGHT(inst)))
#define variant_base(op) ((op) >= ADD_SS ? ADD + ((op) - ADD_SS) / 9 : (op))

#define is_int_p(v) is_int(*(v))
#define is_num_p(v) (is_int_p(v) || is_float(*(v)))
#define num_p(v) (is_int_p(v) ? (SnapFloat)as_int(*(v)) : as_float(*(v)))

// Whether a float truncates to a 63-bit int (false for NaN)
#define in_int_range(f) ((f) >= (SnapFloat)SNAP_INT_MIN && (f) < -(SnapFloat)SNAP_INT_MIN)

// Integer ops on tagged words (2x + 1). They wrap on overflow, done unsigned
// to avoid undefined behavior. 63-bit division can't overflow an int64_t.
#define untag(x) ((SnapInt)(x) >> 1)
#define tag(x) (((uint64_t)(x) << 1) | 1)
#define add_iop(x, y) ((x) + (y) - 1)
#define sub_iop(x, y) ((x) - (y) + 1)
#define mul_iop(x, y) ((uint64_t)untag(x) * ((y) - 1) + 1)
#define div_iop(x, y) tag(untag(x) / untag(y))
#define mod_iop(x, y) tag(untag(x) % untag(y))

#define add_fop(x, y) ((x) + (y))
#define sub_fop(x, y) ((x) - (y))
#define mul_fop(x, y) ((x) * (y))
#define div_fop(x, y) ((x) / (y))
#define mod_fop(x, y) fmod(x, y)

#define int_zero_divisor() (is_int_p(b) && as_int(*b) == 0 && is_int_p(a))

// apply's last argument: a vector, a list or nil
#define is_seq(v) (is_vec(v) || is_cons(v) || is_nil(v))

// Sets *f to a number as a float, checking its kind once. Returns false if it
// isn't a number.
static inline bool num_val(const SValue* v, SnapFloat* f) {
  if (is_imm_float(*v)) *f = snap_imm_float_val(*v);
  else if (is_int(*v)) *f = (SnapFloat)as_int(*v);
  else if (is_obj_type(*v, STYPE_FLOAT)) *f = ((SFloat*)as_obj(*v))->f;
  else return false;
  return true;
}

static inline int seq_length(SValue seq) {
  int n = 0;
  if (is_vec(seq)) return as_vec(seq)->len;
  for (; !is_nil(seq) && !is_empty(seq); seq = as_cons(seq)->rest) n++;
  return n;
}

#endif // SNAP_SNAP_VM_H
