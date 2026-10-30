#CFLAGS=-g -O0 -Wall -Wextra -Wno-unused-parameter -Wno-unused-function
# -fno-crossjumping keeps a separate dispatch jump per opcode in exec() (GCC
# otherwise merges them, which hurts branch prediction)
CFLAGS=-O3 -fno-crossjumping
LDFLAGS=-lm -lreadline

TARGET=snap
SOURCES=$(wildcard *.c)
OBJECTS=$(patsubst %.c,%.o,$(SOURCES))

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJECTS) $(LDFLAGS)

ifneq ($(MAKECMDGOALS),clean)
-include $(SOURCES:%.c=.depends/%.d)
endif

# The generated headers must exist before the dependencies can be found
.depends/%.d: %.c snap_lex.h repl.h | .depends
	$(CC) $(CFLAGS) -MM -c $< -o $@

.depends:
	mkdir .depends

snap_lex.h: snap_lex.rh
	ragel snap_lex.rh

# The REPL is written in snap and built in as a C string
repl.h: repl.snap
	sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/^/"/' -e 's/$$/\\n"/' repl.snap > repl.h

test: $(TARGET)
	@tests/run.sh ./$(TARGET)

# Collect every few bytes under ASan/UBSan to shake out missing GC roots
stress: snap_lex.h repl.h
	$(CC) -g -O0 -fsanitize=address,undefined -DGC_HEAP_GROWTH=1 -DGC_MIN_STEP=256 -o snap_stress $(SOURCES) $(LDFLAGS)
	@for f in test.snap closure.snap loop.snap new.snap; do \
		echo "== $$f"; ./snap_stress $$f > /dev/null || exit 1; \
	done
	@tests/run.sh ./snap_stress

.PHONY: clean test stress
clean:
	rm -rf $(TARGET).dSYM .depends
	rm -f $(TARGET) snap_stress snap_lex.h repl.h *.o a.out
