# Snap

A weird Clojure-ish lisp that I built to write garbage collectors for and to
sort of learn functional programming. The goal is a very small VM and GC that
are fast, small and simple.

## Features

* Closures
* Macros (`defmacro` with quasiquote)
* Structs, methods and interfaces, in the style of Go
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

Special forms: `def`, `defmacro`, `set!`, `fn`, `let`, `loop`, `recur`, `if`, `begin`,
`quote`, `and`, `or`, `not`, `try`, `panic`, `recover`, and the operators
`+ - * / %` and `< <= > >= =`. `(- x)` negates.

The operators and `not` can be used as values too, as in `(reduce + 0 v)` or
`(map not v)`: there they're C functions that do the same thing (a call like
`(+ a b)` still compiles to an instruction). The others can't, and none of
the special forms can be rebound.

A function's last parameter can take the rest of its arguments, as a list:
`(fn [a & more] ...)` called with `1 2 3` gets `a` = 1 and `more` = `(2 3)`.

`let` and `loop` bindings alternate names and values, and are bound in order,
so each value can use the names before it: `(let [a 1 b (+ a 1)] ...)`.

Values are 63-bit integers, floats, strings, keywords (`:name`), symbols,
vectors, maps, lists, structs, types, `nil`, `true`, `false`, functions and
errors. `nil` and `false` are false, everything else is true. Integer
arithmetic wraps on overflow, and mixing integers with floats gives a float.
Strings can contain the escapes `\n`, `\t`, `\r`, `\\` and `\"`. Integers
can be written in hex, as in `0xff`.

Built-in functions:

* Output and input: `print`, `println` and `(read-line prompt?)`, which
  returns `nil` at the end of input. On a terminal it uses readline and shows
  the prompt. It's defined in `main.c`, so programs that embed snap don't need
  readline.
* Predicates: `nil?`, `vector?`, `map?`, `list?` and `sym?`
* Errors: `err`, `err?`, `err-kind` and `err-msg`
* Lists: `list`, `cons`, `first`, `rest`, `concat` (of lists and vectors)
  and `vec` (a list to a vector). `count` and `nth` work on lists too. `nil`
  counts as an empty list.
* Macros: `gensym` and `macroexpand` (see below)
* Types: `type`, `make-type`, `add-method!` and `find-method` (see Structs
  below)
* Vectors and maps: `count`, `nth`, `push!`, `pop!`, `put!`, `get`, `del!`,
  `keys` and `vals`
* Strings: `(str x...)` joins strings, and other values as they print
* Math: `sqrt`, `sin`, `cos` and `(atan2 y x)` give floats; `floor` and `int`
  (toward zero) give integers; `float`, `abs`, `min` and `max`; `(rand)` is a
  float from 0 up to 1 and `(rand n)` an integer from 0 up to `n`
* Code: `(compile src)` compiles a string into a function without arguments.
  It returns an error if the code doesn't parse or compile (`:snap_eof_error`
  when it ends early), and `nil` if there's no code.
* GC: `(gc-step work)` does about `work` units of collection (marking a value
  or sweeping an object is one), starting a collection if none is running,
  and returns `true` when a collection finishes. `(gc-step 0)` just starts one.
  `(gc-stats)` is a map of what the collector has done: `:collections`
  finished, `:steps` (pauses, each starting a collection or doing a step of
  one), `:allocated` (bytes, in all), `:pause-total`, `:pause-max`,
  `:pause-mean`, and `:pause-median`, `:pause-p75` and `:pause-p90` (the
  50th, 75th and 90th percentiles of the latest 64 pauses), all in seconds,
  and the heap now: `:heap` and `:next-gc` (bytes) and `:phase`
  (`:idle`, `:mark` or `:sweep`). In C they're in `snap->gc_stats`.

The standard library, `stdlib.snap`, is written in snap and built in. It has
`defn`, `defconst`, `when`, `cond`, `dotimes`, `(doseq [x coll] ...)`, `inc`,
`dec`, `map`, `filter`, `reduce` and `(sort-by key coll)` (which take a
vector or list and make a vector), and the struct macros below.

`(defconst name expr)` defines a global that can't change: `def`, `set!` or
another `defconst` of it is an error. Code compiled after it uses the value
itself in place of the global, so `(* x SCALE)` is as fast as `(* x 3)`.
It's built on `(set-const! 'name val)`.

```clojure
(defn sum [v] (reduce (fn [a b] (+ a b)) 0 v))
(dotimes [i 3] (println i))
(cond (< x 0) :neg (= x 0) :zero true :pos)
```

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

## Structs, methods and interfaces

A struct has a fixed set of fields. Methods are defined outside it, and an
interface is a set of methods that any type with all of them satisfies,
without saying so, like Go.

```clojure
(defstruct Rect [w h])
(defstruct Circle [r])

(def a (Rect 3 4))      ; A value for each field, in order
(get a :w)              ; => 3
(put! a :w 10)          ; => #Rect{:w 10 :h 4}
(type a)                ; => Rect
(= (Rect 1 2) (Rect 1 2)) ; => true, compared by contents

; The first parameter is the receiver, with its type
(defmethod area [r Rect] (* (get r :w) (get r :h)))
(defmethod area [c Circle] (* 3.14159 (get c :r) (get c :r)))
(area a)                ; => 40, picked by the type of the first argument

(definterface Shape area)
(satisfies? Shape a)    ; => true
(satisfies? Shape 5)    ; => false
```

`get` or `put!` with a field the struct doesn't have panics, as does calling
a method on a type it has no implementation for. A method can have more
parameters after the receiver, and defining it again for a type replaces it.
A method is a value like a function, so `(map area shapes)` works.

Every value has a type, and the built-in ones can have methods too: `Nil`,
`Bool`, `Int`, `Float`, `Str`, `Sym`, `Key`, `Err`, `List`, `Vector`, `Map`,
`Type` and `Fn`.

```clojure
(defmethod area [v Vector] (reduce (fn [t s] (+ t (area s))) 0 v))
(area [(Rect 1 2) (Circle 1)]) ; => 5.14159
```

Underneath, `defstruct` calls `(make-type 'Rect '[w h])`, `defmethod` calls
`(add-method! 'area Rect (fn [r] ...))`, which defines the method if the name
isn't defined yet, and `(find-method area Rect)` is the function a type runs,
or `nil`. An interface is just a vector of method names.

## Macros

A macro is a function that runs while the code is being compiled. It's given
the forms it's called with, unevaluated, and returns a form that's compiled in
place of the call.

```clojure
(defmacro unless [c & body] `(if ~c nil (begin ~@body)))
(unless (= x 0) (println "not zero") x)
; is compiled as
(if (= x 0) nil (begin (println "not zero") x))
```

Quasiquote (`` ` ``) is like `quote` but makes a new form each time, with
`~x` replaced by the value of `x` and `~@x` by the values in the list (or
vector) `x`. Symbols and special forms in it are quoted.

`(gensym)` makes a new symbol (`#g1`, `#g2`...) that can't clash with any in
the code, for the macro's own variables:

```clojure
(defmacro swap! [a b]
  (let [t (gensym)] `(let [~t ~a] (set! ~a ~b) (set! ~b ~t))))
```

`(macroexpand form)` shows what a macro call becomes:
`(macroexpand '(unless c a))` gives `(if c nil (begin a))`.

Macros are global, and `defmacro` can only be used at the top level. A local
variable with a macro's name hides it. A file runs one top-level form at a
time, so a macro can be used by the forms after it, and its code can call the
functions defined before it. (Code given to `compile` is compiled all at once,
so a macro it defines can only be used by later code.) A macro that panics,
or returns an error, makes the code that calls it fail to compile.

Macros aren't hygienic: a macro's code refers to functions by name, so a local
with the same name at the call would be used instead. Special forms can't be
hidden, so `if`, `let`, `fn` and the rest are safe, as are the functions that
quasiquote uses.

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
division by zero and calls with the wrong number of arguments. A top-level
form whose value is an error (such as from `try`) also stops a script.

Recursing without end panics with `:snap_stack_error` once the stack holds
a million values, as in Lua, rather than using up memory. Recursion in C is
limited to 1000 levels, so a macro that compiles code that uses itself, or
code nested 1000 deep, is an error rather than a crash. The limits are
`SNAP_MAX_STACK` and `SNAP_MAX_C_DEPTH` in `snap.h`.

`(recover f)` is the one place a panic stops. It calls `f` without arguments,
and if `f` panics, the panic's error is the result instead. It's meant for
code that supervises other code, like the REPL:

```clojure
(recover (fn [] (/ 1 0)))   ; => <err :snap_arith_error "Division by zero">
(recover (fn [] 42))        ; => 42
```

C functions return `false` to panic, with `*result` set to the error. To
return an error as a value, set `*result` to it and return `true`.

## Holding values in C

The collector sees the stack, the globals, and what they reach, but not C
variables. A C function's arguments are safe for the whole call. A value C
keeps after the call returns, such as a callback it calls every frame or a
map in a C global, needs a pin:

```c
SValue on_frame = snap_pin(snap, args[0]); // Kept until unpinned
...
snap_unpin(snap, on_frame);
```

Pins can be unpinned in any order, and a value pinned twice needs two unpins.
Errors don't undo them. Inside snap.c, `snap_anchor()` and `snap_release()`
do the short-lived version of this: they keep an object alive while C is
still building it (filling a new map allocates), and they come undone, last
first, by the end of the call or when an error unwinds.

## Implementation

Everything is in `snap.c`. The lexer is in `snap_lex.rh`, and `main.c` runs a
file or the REPL. The REPL is `repl.snap`, which the Makefile turns into a C
string in `repl_snap.h`: it reads lines until `compile` stops returning
`:snap_eof_error`, then calls the result with `recover` and prints it.
`stdlib.snap` is built in the same way, as `stdlib_snap.h`, and `snap_init()` runs
it.

* **Values** are 8 bytes. The low bits tag integers, floats, objects,
  `nil`/booleans and C functions, so integers and most floats need no
  allocation. A float is stored in the value, as in Spur (Smalltalk), when its
  magnitude is between about 1e-38 and 6e38, or it's zero: its bits are
  rotated and its exponent offset to make room for the tag, keeping the whole
  mantissa, so it's exact. Other floats (and infinities and NaN) are boxed.
* **Macros** are functions with a flag set by `defmacro`. When the compiler
  sees a call to a global that's a macro, it calls it (with `snap_call()`,
  which can run snap code from C, even from inside a C function) and
  compiles the result. Quasiquote is expanded by the reader into calls to
  `list` and `concat`, which go in the code as the functions themselves
  rather than their names.
* **Structs** are a pointer to their type and a value for each field.
  Fields are found by comparing the keyword with the type's fields, which is
  fast for structs of normal size. Types are never freed, like symbols. A
  method holds a map from types to functions, and calling one looks up the
  first argument's type and calls what it finds in its place.
* **The compiler** turns each form straight into byte code, one 32-bit
  instruction at a time, with a few peepholes. Binary operators can read
  locals and constants directly and store arithmetic results straight into a
  local, so most of a tight loop doesn't touch the stack. A call to a global
  is one instruction that carries the name. Calls to `nth`, `count`, `get` and
  `int` have instructions of their own that index vectors, look up maps and
  struct fields, and truncate floats themselves, as long as the globals still
  hold the built-ins (otherwise, and for other arguments, they call the
  global). An expression whose value isn't
  used (all but the last in a body) is compiled without its value, so an `if`
  or `when` there doesn't push `nil` for a missing branch only to pop it.
* **The VM** is a stack machine with computed-goto dispatch. Once a function
  is compiled, each binary op is switched to a variant for its kinds of
  operands (stack, local or constant), which handles ints without decoding
  them and falls back to the general handler otherwise (for floats, without
  fetching the operands again). Counting loops end in
  a single `STEP` instruction that adds to the counter, compares it and jumps
  back. Closed-over variables are shared between closures and closed when
  their frame returns.
* **The stack** is one array for every frame, which grows (by reallocating
  and fixing the pointers into it) up to `SNAP_MAX_STACK`. It only moves when
  a function is called, so the VM reloads its pointers after a C function
  only if that C function ran snap code through `snap_call()`, which sets a
  flag, as Lua 5.4 does.
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
* **Allocation** takes constant time for objects up to 256 bytes (nearly
  all of them): they come from free lists of slots of 8 sizes, carved from
  16KB pages that are kept for reuse. Bigger objects get a page of their own.
  Sweeping walks the pages slot by slot, so objects need no list pointer and
  their header is 8 bytes. Big objects (such as long strings) and the arrays
  inside vectors and maps still come from `malloc`.
* **Vectors** never copy their values to grow. A vector keeps the array it
  was made with (a literal's is exactly its size), and values pushed past it
  go in segments of 8, 16, 32 and so on. `nth` finds the segment with a
  count-leading-zeros instruction. `pop!` frees the last segment once the one
  before it is empty too. In C, use `snap_vec_at()` rather than `data`.
* **Real-time programs** can set `gc_manual` in the `Snap` struct so that
  allocation never collects, and call `snap_gc_step()` (or `gc-step`) in
  their idle time, such as the end of each frame.

## TODO

* Growing a map copies it, which takes time in proportion to its size
