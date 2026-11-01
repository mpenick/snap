#CFLAGS=-g -O0 -Wall -Wextra -Wno-unused-parameter -Wno-unused-function
# -fno-crossjumping keeps a separate dispatch jump per opcode in exec() (GCC
# otherwise merges them, which hurts branch prediction). -DNDEBUG drops the
# asserts, including the type checks in the as_ macros, which the stress build
# keeps.
CFLAGS=-O3 -fno-crossjumping -DNDEBUG
LDFLAGS=-lm -lreadline

TARGET=snap
SOURCES=$(wildcard *.c)
OBJECTS=$(patsubst %.c,%.o,$(SOURCES))

# The core, without main.c's REPL. Embedders link libsnap.a.
CORE=snap.c snap_gc.c snap_vm.c snap_parse.c snap_compile.c snap_builtins.c
CORE_OBJECTS=$(patsubst %.c,%.o,$(CORE))

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJECTS) $(LDFLAGS)

libsnap.a: $(CORE_OBJECTS)
	$(AR) rcs $@ $(CORE_OBJECTS)

ifneq ($(MAKECMDGOALS),clean)
-include $(SOURCES:%.c=.depends/%.d)
endif

# The generated headers must exist before the dependencies can be found
.depends/%.d: %.c snap_lex.h repl_snap.h stdlib_snap.h | .depends
	$(CC) $(CFLAGS) -MM -c $< -o $@

.depends:
	mkdir .depends

snap_lex.h: snap_lex.rh
	ragel snap_lex.rh

# The REPL and the standard library are written in snap and built in as C
# strings
%_snap.h: %.snap
	sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/^/"/' -e 's/$$/\\n"/' $< > $@

# tests/api.c tests the C API, which scripts can't reach
test: $(TARGET) tests/api
	@tests/run.sh ./$(TARGET)
	@tests/api

tests/api: tests/api.c $(CORE) snap.h snap_internal.h stdlib_snap.h snap_lex.h
	$(CC) $(CFLAGS) -I. -o $@ tests/api.c $(CORE) -lm

# Collect constantly, a few units of work per allocation, under ASan/UBSan to
# shake out missing GC roots and barriers. Each object gets its own malloc()
# so that ASan sees it freed; then the tests run again with the slot pools.
STRESS_FLAGS=-g -O0 -fsanitize=address,undefined -DGC_HEAP_GROWTH=1 -DGC_MIN_STEP=256 -DGC_STEP_SIZE=1 -DGC_STEP_MUL=1
stress: snap_lex.h repl_snap.h stdlib_snap.h
	$(CC) $(STRESS_FLAGS) -DGC_POOL=0 -o snap_stress $(SOURCES) $(LDFLAGS)
	$(CC) $(STRESS_FLAGS) -o snap_stress_pool $(SOURCES) $(LDFLAGS)
	$(CC) $(STRESS_FLAGS) -DGC_POOL=0 -I. -o tests/api_stress tests/api.c $(CORE) -lm
	@for f in examples/test.snap examples/closure.snap examples/loop.snap examples/new.snap; do \
		echo "== $$f"; ./snap_stress $$f > /dev/null || exit 1; \
	done
	@tests/run.sh ./snap_stress
	@tests/run.sh ./snap_stress_pool
	@tests/api_stress

.PHONY: clean test stress
clean:
	rm -rf $(TARGET).dSYM .depends
	rm -f $(TARGET) snap_stress snap_stress_pool tests/api tests/api_stress libsnap.a snap_lex.h repl_snap.h stdlib_snap.h *.o a.out
