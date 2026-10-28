#!/bin/sh
# Usage: tests/run.sh <snap binary>
# Runs each tests/**/*.snap and compares stdout+stderr with its .out file.

bin=${1:-./snap}
dir=$(dirname "$0")
pass=0
fail=0

for test in "$dir"/*.snap "$dir"/errors/*.snap; do
  expected="${test%.snap}.out"
  if "$bin" "$test" 2>&1 | diff -u "$expected" - > /dev/null; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    echo "FAIL: $test"
    "$bin" "$test" 2>&1 | diff -u "$expected" - | head -20
  fi
done

echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ]
