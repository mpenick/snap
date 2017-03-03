#!/bin/bash

export DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib
export MALLOC_STRICT_SIZE=1
export MALLOC_FILL_SPACE=1
./snap $1
