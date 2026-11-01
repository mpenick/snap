#!/bin/sh
# Usage: tests/vm/check.sh [cc]  (make vmcheck)
#
# Checks that exec() isn't fragile: that adding opcodes to the VM doesn't
# change how fast it dispatches. The compiler's register allocation for
# exec() can tip, with a change as small as one more opcode, into keeping pc
# in memory, which makes the dispatch loop about 30% slower. So this builds
# snap as it is and with 1, 2 and 3 extra opcodes (handlers that make a call,
# as real ones do), runs tests/vm/bench.snap on each (best of 5), and fails
# if any differs from the unchanged build by more than 10%. It compares
# builds on this machine, so it needs no recorded baseline.

cc=${1:-cc}
src=$(cd "$(dirname "$0")/../.." && pwd)
bench="$src/tests/vm/bench.snap"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
limit=10

build() { # build <dir> <extra opcodes>
  mkdir -p "$1"
  cp "$src"/*.c "$src"/*.h "$src"/*.rh "$src"/*.snap "$src"/Makefile "$1"/ 2>/dev/null
  awk -v n="$2" '{ print } /^  XX\(CALL_GLOBAL\) \\$/ { for (i = 0; i < n; ++i) printf "  XX(VMCHECK%d) \\\n", i }' \
    "$src/snap_vm.h" > "$1/snap_vm.h"
  awk -v n="$2" '/^      OP\(CALL_GLOBAL\) \{$/ { for (i = 0; i < n; ++i)
      printf "      OP(VMCHECK%d) { if (arg == 12345 + %d) { frame->stack_top = stack; snap_run_finalizers(snap); *stack++ = locals[arg]; } DISPATCH(); }\n", i, i }
    { print }' "$src/snap_vm.c" > "$1/snap_vm.c"
  (cd "$1" && make CC="$cc" snap > build.log 2>&1) || { cat "$1/build.log"; exit 1; }
}

best() { # best <snap binary>: the fastest of 5 runs
  for i in 1 2 3 4 5; do "$1" "$bench"; done | sort -n | head -1
}

build "$work/base" 0
base=$(best "$work/base/snap")
echo "$cc: unchanged ${base}s"
status=0
for n in 1 2 3; do
  build "$work/plus$n" $n
  t=$(best "$work/plus$n/snap")
  pct=$(awk -v t="$t" -v b="$base" 'BEGIN { printf "%+.1f", (t - b) / b * 100 }')
  if awk -v p="$pct" -v l="$limit" 'BEGIN { exit !(p > l || p < -l) }'; then
    echo "$cc: $n more opcodes ${t}s (${pct}%)  FAIL: more than ${limit}% different"
    status=1
  else
    echo "$cc: $n more opcodes ${t}s (${pct}%)"
  fi
done
[ $status -eq 0 ] && echo "$cc: exec() is stable"
exit $status
