UNITY_CLAGS=
CFLAGS=-g -O0 -Wno-unused-function -Wall -Werror -std=gnu11

all: test_bitmap test_alloc

test_bitmap: CFLAGS+=-Ivendor/Unity/src
test_bitmap: test_bitmap.o gc.o vendor/Unity/src/unity.o
	gcc -o test_bitmap test_bitmap.o gc.o vendor/Unity/src/unity.o

test_alloc: CFLAGS+=-Ivendor/Unity/src
test_alloc: test_alloc.o gc.o vendor/Unity/src/unity.o
	gcc -o test_alloc test_alloc.o gc.o vendor/Unity/src/unity.o

gc.o: gc.h mstructs.h

clean:
	rm -f gc test_bitmap *.o
