#!/bin/bash

LLDB_FILE=$(mktemp)

cat > $LLDB_FILE <<- EOM
env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib
env MALLOC_STRICT_SIZE=1
env MALLOC_FILL_SPACE=1
run
EOM

DEBUG="-f ./snap -s $LLDB_FILE -- $1"
lldb $DEBUG
