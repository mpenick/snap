CFLAGS=-g -O0

TARGET=snap
SOURCES=$(filter-out snap_lex.c, $(wildcard *.c)) snap_lex.c
OBJECTS=$(patsubst %.c,%.o,$(SOURCES))

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJECTS) $(LDFLAGS)

ifneq ($(MAKECMDGOALS),clean)
-include $(SOURCES:%.c=.depends/%.d)
endif

.depends/%.d: %.c .depends
	$(CC) $(CFLAGS) -MM -c $< -o $@

.depends:
	mkdir .depends

snap_lex.c: snap_lex.rl
	ragel snap_lex.rl

.PHONY: clean
clean:
	rm -rf $(TARGET).dSYM .depends
	rm -f $(TARGET) *.o snap_lex.c a.out
