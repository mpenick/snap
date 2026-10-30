# Snap

A weird Clojure-ish lisp that I built to write garbage collectors for and to
sort of learn functional programming. The goal is a very small VM and GC that
are fast, small and simple.

## Features

* Closures
* Tail recursion (`recur` and `loop`)
* Byte code compiler and stack-based interpreter
* Incremental mark-and-sweep GC with short pauses
* FFI (C functions)
* Errors as values (no exceptions)

## Building and running

Building needs a C compiler, [Ragel](https://www.colm.net/open-source/ragel/)
(for the lexer) and GNU Readline (for the REPL, `libreadline-dev` on Debian and
Ubuntu).

```
make
./snap [-d] [<file> <arg_1>...<arg_n>]
```

Without a file, `snap` starts a REPL. A form can span several lines, and
definitions carry over. Errors and panics are printed without ending the
session. Line editing and history come from readline. Ctrl-D exits. The REPL
is written in snap, in `repl.snap` (see below).

`-d` prints the byte code before running it. The script's arguments are a
vector of strings in `args`.

```
make test     # Run tests/ and compare with the expected output
make stress   # The same under ASan/UBSan, collecting all the time
```

## Language

```clojure
(def fib (fn [n]
  (loop [a 0 b 1 i 0]
    (if (= i n) a (recur b (+ a b) (+ i 1))))))

(println (fib 50))
```

Special forms: `def`, `set!`, `fn`, `let`, `loop`, `recur`, `if`, `begin`,
`quote`, `and`, `or`, `not`, `try`, `panic`, `recover`, and the operators
`+ - * / %` and `< <= > >= =`.

`let` and `loop` bindings alternate names and values, and are bound in order,
so each value can use the names before it: `(let [a 1 b (+ a 1)] ...)`.

Values are 63-bit integers, floats, strings, keywords (`:name`), symbols,
vectors, maps, lists (only from `quote`), `nil`, `true`, `false`, functions and
errors. `nil` and `false` are false, everything else is true. Integer
arithmetic wraps on overflow, and mixing integers with floats gives a float.
Strings can contain the escapes `\n`, `\t`, `\r`, `\\` and `\"`.

Built-in functions:

* Output and input: `print`, `println` and `(read-line prompt?)`, which
  returns `nil` at the end of input. On a terminal it uses readline and shows
  the prompt. It's defined in `main.c`, so programs that embed snap don't need
  readline.
* Types: `nil?`, `vector?` and `map?`
* Errors: `err`, `err?`, `err-kind` and `err-msg`
* Vectors and maps: `count`, `nth`, `push!`, `pop!`, `put!`, `get`, `del!`,
  `keys` and `vals`
* Strings: `(str s...)` joins strings
* Code: `(compile src)` compiles a string into a function without arguments.
  It returns an error if the code doesn't parse or compile (`:snap_eof_error`
  when it ends early), and `nil` if there's no code.
* GC: `(gc-step work)` does about `work` units of collection (marking a value
  or sweeping an object is one), starting a collection if none is running,
  and returns `true` when a collection finishes. `(gc-step 0)` just starts one.

## Vectors

Vectors are mutable and grow as needed. Functions that change one end in `!`.

```clojure
(def v [1 2 3])     ; A literal makes a new vector each time it's evaluated
(count v)           ; => 3
(nth v 0)           ; => 1
(push! v 4 5)       ; => [1 2 3 4 5], appends and returns v
(pop! v)            ; => 5, removes and returns the last value
(put! v 0 :a)       ; => [:a 2 3 4], replaces and returns v
(= [1 [2]] [1 [2]]) ; => true, compared by contents
```

An index that's out of range, or popping an empty vector, panics.

## Maps

Maps are mutable hash maps that keep insertion order.

```clojure
(def m {:a 1 :b 2}) ; A literal makes a new map each time it's evaluated
(get m :a)          ; => 1
(get m :z)          ; => nil
(get m :z 0)        ; => 0, the default for a missing key
(put! m :c 3)       ; => {:a 1 :b 2 :c 3}, adds or replaces and returns m
(del! m :a)         ; => {:b 2 :c 3}, removes and returns m
(keys m)            ; => [:b :c]
(vals m)            ; => [2 3]
(count m)           ; => 2
(= {:a 1 :b 2} {:b 2 :a 1}) ; => true, compared by contents
```

Any value can be a key. Vectors and maps are compared by contents, so don't
change one after using it as a key.

Vectors and maps inside a quoted value, like `'[1 2]`, are read-only. A quoted
value is a single constant shared by every evaluation, so changing one panics.

## Errors

There are no exceptions. An error is an ordinary value that's returned and
checked like any other:

```clojure
(def safe-div (fn [a b]
  (if (= b 0) (err :div-by-zero "can't divide by zero") (/ a b))))

(def r (safe-div 1 0))
(err? r)                ; => true
(err? r :div-by-zero)   ; => true, it's an error of this kind
(err-kind r)            ; => :div-by-zero
(err-msg r)             ; => "can't divide by zero"
```

`(try expr)` passes errors up: if `expr` is an error, the enclosing `fn`
returns it right away, otherwise `try` gives its value. Every place an error
can come out of is marked in the source.

```clojure
(def calc (fn [a b c]
  (let [x (try (safe-div a b))]
    (+ x (try (safe-div x c))))))

(calc 100 10 5)   ; => 12
(calc 100 0 5)    ; => <err :div-by-zero "can't divide by zero">

; Handle one kind and pass the rest up
(def div-or-zero (fn [a b]
  (let [r (safe-div a b)]
    (if (err? r :div-by-zero) 0 (try r)))))
```

`(panic kind msg)` is for bugs. It stops the program, as do type errors,
division by zero and calls with the wrong number of arguments. An error that
`try` returns from the top level of a script also stops it.

`(recover f)` is the one place a panic stops. It calls `f` without arguments,
and if `f` panics, the panic's error is the result instead. It's meant for
code that supervises other code, like the REPL:

```clojure
(recover (fn [] (/ 1 0)))   ; => <err :snap_arith_error "Division by zero">
(recover (fn [] 42))        ; => 42
```

C functions return `false` to panic, with `*result` set to the error. To
return an error as a value, set `*result` to it and return `true`.

## Implementation

Everything is in `snap.c`. The lexer is in `snap_lex.rh`, and `main.c` runs a
file or the REPL. The REPL is `repl.snap`, which the Makefile turns into a C
string in `repl.h`: it reads lines until `compile` stops returning
`:snap_eof_error`, then calls the result with `recover` and prints it.

* **Values** are 8 bytes. The low bits tag integers, objects, `nil`/booleans
  and C functions, so integers need no allocation. Floats are heap objects.
* **The compiler** turns each form straight into byte code, one 32-bit
  instruction at a time, with a few peepholes. Binary operators can read
  locals and constants directly and store arithmetic results straight into a
  local, so most of a tight loop doesn't touch the stack.
* **The VM** is a stack machine with computed-goto dispatch. Once a function
  is compiled, each binary op is switched to a variant for its kinds of
  operands (stack, local or constant), which handles ints without decoding
  them and falls back to the general handler otherwise. Counting loops end in
  a single `STEP` instruction that adds to the counter, compares it and jumps
  back. Closed-over variables are shared between closures and closed when
  their frame returns.
* **The GC** is incremental mark and sweep, so pauses stay short however big
  the heap is. A collection starts once the heap has grown to twice what was
  live after the last one, then runs in small steps that allocation pays for.
  Allocating 4KB does a step of about 2,000 units, which takes well under a
  millisecond. Marking uses a snapshot-at-the-beginning write barrier: while
  marking, a value removed from the heap (by `set!`, `put!`, `pop!` or
  `del!`) is marked first, so changes to the heap can't hide a live object.
  The stack is marked all at once when a collection starts, so stack writes,
  which are most of what the VM does, don't need the barrier. Objects made
  during a collection survive it. Big vectors and maps are marked 64 values at
  a time, and sweeping goes a few objects at a time. Which mark value means
  "marked" flips each collection, so objects never need to be unmarked.
* **Real-time programs** can set `gc_manual` in the `Snap` struct so that
  allocation never collects, and call `snap_gc_step()` (or `gc-step`) in
  their idle time, such as the end of each frame.

## TODO

* Allocate objects from pools of fixed sizes, so that allocation has bounded
  time too (`malloc` and `free` don't)
* Floats in values instead of on the heap
