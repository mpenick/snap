TARGET=a

all: main.c lex.c
	gcc -o ${TARGET} -g main.c lex.c

lex.c: lex.rl lex.h
	ragel lex.rl

clean:
	rm -rf ${TARGET}.dSYM
	rm -f ${TARGET} lex.c
