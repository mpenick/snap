UNITY_CLAGS=
CFLAGS=-g -O0 -Wno-unused-function -Wall -Werror -std=gnu11

all: test_bitmap test_alloc test_header test_tagged

test_bitmap: CFLAGS+=-Ivendor/Unity/src
test_bitmap: gc.h test_bitmap.o gc.o vendor/Unity/src/unity.o
	gcc -o test_bitmap test_bitmap.o gc.o vendor/Unity/src/unity.o

test_alloc: CFLAGS+=-Ivendor/Unity/src
test_alloc: gc.h test_alloc.o gc.o vendor/Unity/src/unity.o
	gcc -o test_alloc test_alloc.o gc.o vendor/Unity/src/unity.o

test_header: CFLAGS+=-Ivendor/Unity/src
test_header: gc.h test_header.o gc.o vendor/Unity/src/unity.o
	gcc -o test_header test_header.o gc.o vendor/Unity/src/unity.o

test_tagged: CFLAGS+=-Ivendor/Unity/src
test_tagged: gc.h test_tagged.o gc.o vendor/Unity/src/unity.o
	gcc -o test_tagged test_tagged.o gc.o vendor/Unity/src/unity.o

gc.o: gc.h mstructs.h

clean:
	rm -f gc test_bitmap *.o
