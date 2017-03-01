CFLAGS=-g -O0 -Wall -Wextra -Wno-unused-parameter -Wno-unused-function
#CFLAGS=-O3

TARGET=snap
SOURCES=$(wildcard *.c)
OBJECTS=$(patsubst %.c,%.o,$(SOURCES))

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJECTS) $(LDFLAGS)

ifneq ($(MAKECMDGOALS),clean)
-include $(SOURCES:%.c=.depends/%.d)
endif

.depends/%.d: snap_lex.h %.c .depends
	$(CC) $(CFLAGS) -MM -c $< -o $@

.depends:
	mkdir .depends

snap_lex.h: snap_lex.rh
	ragel snap_lex.rh

.PHONY: clean
clean:
	rm -rf $(TARGET).dSYM .depends
	rm -f $(TARGET) snap_lex.h *.o a.out
