// The compiler (snap_compile.c)
#ifndef SNAP_SNAP_COMPILE_H
#define SNAP_SNAP_COMPILE_H

#include "snap_parse.h"

SValue compile_forms(Snap* snap, SnapLex* lex, bool all);
const char* form_name(int token);
void gc_mark_fn_states(Snap* snap);
void compile_init_inline(Snap* snap);
SValue macro_expand(Snap* snap, SValue macro, SCons* args);

#endif // SNAP_SNAP_COMPILE_H
