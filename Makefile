TARGET=a

all: snap

snap: snap.c snap_lex.c
	gcc -o ${TARGET} -g snap.c snap_lex.c

snap_lex.c: snap_lex.rl snap_lex.h
	ragel snap_lex.rl

clean:
	rm -rf ${TARGET}.dSYM
	rm -f ${TARGET} lex.c a.out
