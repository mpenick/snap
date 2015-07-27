#!/bin/bash

export DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib
export MALLOC_STRICT_SIZE=1
export MALLOC_FILL_SPACE=1
./snap $1

#LLDB_FILE="/tmp/cpp_driver_lldb_commands"
#LLDB_CMDS="env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib;
#env MALLOC_STRICT_SIZE=1;
#env MALLOC_FILL_SPACE=1;"
#echo $LLDB_CMDS > $LLDB_FILE
#DEBUG="lldb -s $LLDB_FILE -- ./snap"
#lldb $DEBUG
