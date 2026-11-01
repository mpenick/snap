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
* Incremental mark-and-sweep GC with short pauses, and finalizers
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
make vmcheck  # Check that the VM stays as fast with more opcodes
```

It builds with GCC or clang (`make CC=clang`), with no compiler-specific
flags.

## Language

```clojure
(def fib (fn [n]
  (loop [a 0 b 1 i 0]
    (if (= i n) a (recur b (+ a b) (+ i 1))))))

(println (fib 50))
```

Special forms: `def`, `defmacro`, `set!`, `fn`, `let`, `loop`, `recur`, `if`, `begin`,
`quote`, `and`, `or`, `not`, `try`, `panic`, `recover`, and the operators
`+ - * / %` and `< <= > >= = not=`. `(- x)` negates, and `(+ x)` is `x` (it must be a number).
`(not= a b ...)` is `(not (= a b ...))`.

The operators and `not` can be used as values too, as in `(reduce + 0 v)` or
`(map not v)`: there they're C functions that do the same thing (a call like
`(+ a b)` still compiles to an instruction). The others can't, and none of
the special forms can be rebound.

A function's last parameter can take the rest of its arguments, as a list:
`(fn [a & more] ...)` called with `1 2 3` gets `a` = 1 and `more` = `(2 3)`,
and called with just `1` gets `more` = `nil`, so `(if more ...)` tells
whether there are any. `nil` works as an empty list (`first`, `rest`,
`next`, `count`, `map`, `doseq`, `concat`, `~@`...).

`let` and `loop` bindings alternate names and values, and are bound in order,
so each value can use the names before it: `(let [a 1 b (+ a 1)] ...)`.
The special forms (`begin def defmacro if fn let loop quote recur set! panic
try recover < <= > >= = not and or + - * / %`) can't be bound, anywhere a name
is; a map pattern can take their key under another name: `{o :or}`.

In `let`, `loop`, `fn` and `def` (so `defn` and `doseq` too), a pattern can
stand in for a name and bind the names in it to parts of the value:

```clojure
(let [[x z] p] ...)                  ; x = (nth p 0), z = (nth p 1)
(let [[a b & more :as all] v] ...)   ; more = (nthnext v 2), all = v
(let [{:keys [x y] :as m} pt] ...)   ; x = (get pt :x), m = pt
(defn dist [[ax az] [bx bz]] ...)
(doseq [[k v] pairs] ...)
(def [bree-x bree-z] BREE)           ; Defines both, as two defs would
```

A map pattern can also bind a name (or a pattern) to any key, and give
defaults, by the name, for values that are `nil` (missing). A default is only
evaluated when it's used:

```clojure
(let [{a0 :a w0 :w} g0 {a1 :a w1 :w} g1] ...)        ; a0 = (get g0 :a)
(let [{[x z] :pos} spec] ...)
(let [{:keys [wall floor] :or {wall :plaster}} room] ...)
```

`_` binds nothing: in a pattern its position isn't read (`[_ _ z]` reads
only `z`), and as a whole binding (`let`, `loop`, `fn`, `doseq`, `dotimes`)
the value is still evaluated but can't be read, so `(fn [_ _ z] z)` is fine.
`(def _ expr)` evaluates `expr` and defines nothing.

Patterns nest. They compile to the `nth` and `get` calls they stand for, so
they're as fast, and a vector too short for its pattern is `nth`'s error.
Map patterns work on structs too, and a missing key is `nil`. A pattern on
`nil` is an error; for a value that can be `nil`, `(if-let [[x z] (find)]
then else)` and `(when-let [[x z] (find)] body...)` bind only when the value
is truthy. In a `loop`,
`recur` takes one value for each binding, as usual: `(recur [b (+ a b)] ...)`.

A function that ends in a vector literal can return several values without
making the vector: when its caller takes the parts straight away, with a
vector pattern (no `&` or `:as`) on the call, the parts go directly to it.

```clojure
(defn rot-yaw [x z yaw]
  (let [c (cos yaw) s (sin yaw)] [(- (* x c) (* z s)) (+ (* x s) (* z c))]))
(let [[rx rz] (rot-yaw x z yaw)] ...)  ; No vector made
(def p (rot-yaw x z yaw))              ; A vector, as usual
```

Nothing else changes: any other caller gets a new vector, and a value that
isn't a returned literal is taken apart as `nth` would. Patterns compiled
while `nth` isn't the built-in use it, as they always did.

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
* Predicates: `nil?`, `vector?`, `map?`, `list?` and `sym?`, and one for each
  other built-in type: `int?`, `float?`, `number?` (either), `str?`, `key?`,
  `bool?`, `fn?` (functions, C functions and methods, but not keywords or
  types, which can be called too), `struct?` (any struct) and `type?`
* Calling: `(apply f a b xs)` calls `f` with `a`, `b` and the values in `xs`
  (a vector, list or `nil`), as in `(apply + [1 2 3])` or passing on rest
  arguments, `(defn log [& args] (apply println :log args))`. The VM spreads
  them itself, so it's an ordinary call. Called from C with `snap_call()`, an
  error value returned by `f` looks like a panic.
* Ranges: `(range n)`, `(range a b)` and `(range a b step)` make a vector of
  the numbers from `a` (0 if not given) up to but not including `b`, by `step`
  (1 if not given, and it can be negative): `(range 2 8 2)` is `[2 4 6]`.
  They're ints if every argument is, otherwise floats. The whole vector is
  made at once, so `dotimes` is still the way to count in a loop.
* Errors: `err`, `err?`, `err-kind`, `err-msg` and `err-where`
* Lists: `list`, `cons`, `first`, `rest`, `next` (like `rest`, but `nil`
  instead of `()` at the end), `nthnext` (the values from an index on, of a
  list or vector, or `nil`), `concat` (of lists and vectors)
  and `vec` (a list, a map's `[k v]` entries, or a copy of a vector, to a vector). `count` and `nth` work on lists too. `nil`
  counts as an empty list.
* Macros: `gensym` and `macroexpand` (see below)
* Types: `type`, `make-type`, `add-method!`, `find-method` and `set-gc!`
  (see Structs and Finalizers below)
* Vectors and maps: `(vector x...)` (a vector literal as a function, as in
  `(map vector xs ys)`), `count`, `nth`, `push!`, `pop!`, `put!`, `get`, `del!`,
  `keys` and `vals`, and `assoc`, `dissoc` and `merge`, which make copies
  (see Maps below)
* Strings: `(str x...)` joins strings, and other values as they print, and
  `(name x)` is a keyword's or symbol's name as a string (`(name :rock)` is
  `"rock"`), and `(format fmt x...)` formats as C's `printf` does:
  `(format "%s-%g.glb" "house" 8.5)` is `"house-8.5.glb"`. Conversions are
  `d i o u x X c` (integers; a float is truncated), `f F e E g G` (numbers)
  and `s` (a string as it is, anything else as `str` prints it), with flags,
  width and precision (`*` takes them from the arguments) and `%%`; no length
  modifiers. Too few or too many arguments is an error.
* Math: `sqrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `exp`, `log`, `(pow x y)`
  and `(atan2 y x)` give floats; `(mod a b)` is the remainder with `b`'s sign,
  so `(mod -1 4)` is 3 where `(% -1 4)` is -1; `floor`, `ceil`, `round` (half away from
  zero) and `int` (toward zero) give integers; `float`, `abs`, `min` and
  `max`; `(rand)` is a float from 0 up to 1 and `(rand n)` an integer from 0
  up to `n`, and `(rand-seed n)` starts them over, to repeat what they give
* Bits: `bit-and`, `bit-or`, `bit-xor`, `(bit-shl x n)` and `(bit-shr x n)`
  (which keeps the sign) of integers, shifting by 0 to 63
* Files: `(load "file")` runs a file and gives its last value, or an error
  (`:snap_io_error` if it can't be read). A relative path is from the
  directory of the file doing the loading, so a program can be split into
  files that load each other. An error from a file loaded by another file
  starts with the file's path. In C it's `snap_load()`.
* Code: `(compile src)` compiles a string into a function without arguments.
  It returns an error if the code doesn't parse or compile (`:snap_eof_error`
  when it ends early), and `nil` if there's no code.
* GC: `(gc-step work)` does about `work` units of collection (marking a value
  or sweeping an object is one, and freeing an array 8 more), starting a collection if none is running,
  and returns `true` when a collection finishes. `(gc-step 0)` just starts one.
  `(gc-stats)` is a map of what the collector has done: `:collections`
  finished, `:steps` (pauses, each starting a collection or doing a step of
  one), `:allocated` (bytes, in all), `:pause-total`, `:pause-max`,
  `:pause-mean`, and `:pause-median`, `:pause-p75`, `:pause-p90` and
  `:pause-p99` (percentiles of the latest 1,024 pauses), all in seconds,
  and the heap now: `:heap` and `:next-gc` (bytes) and `:phase`
  (`:idle`, `:mark` or `:sweep`). In C they're in `snap->gc_stats`.
* Measuring: `(clock)` is seconds of wall time (from a start that doesn't
  move, for timing) and `(cpu-time)` the seconds of CPU the process has used.
  `(alloc-profile! true)` starts counting what's allocated, and
  `(alloc-profile)` stops, returns it and clears it: `[function kind count
  bytes]` rows, most bytes first, such as `["pairs" :vector 300 9600]`.
  `(alloc-profile! false)` pauses it. An allocation counts against the snap
  function running, so one in a built-in (a vector literal, `keys`, `str`)
  counts against its caller. Functions are named by the global that holds
  them: `"f/fn"` for one made in `f`, `"m[Type]"` for a method.
  `"<anonymous>"` is top-level code or a function only data holds, and
  `"<freed>"` code collected since (such as top-level forms, once run).
  Compiling counts too. When it's off it costs one check per allocation.

The standard library, `stdlib.snap`, is written in snap and built in. It has
`defn`, `defconst`, `when`, `cond`, `if-let`, `when-let`, `update!`, `dotimes`,
`(doseq [x coll] ...)` (over a vector, list or map), `inc`, `dec`, `map` (over
one collection or several, as in `(map + xs ys)`, stopping at the shortest),
`filter`, `reduce`, `(keep f coll)` (each `(f x)` that isn't `nil`), `(remove
f coll)` (each `x` where `(f x)` is falsy), `(map-indexed f coll)` (each `(f i
x)`), `(mapcat f coll)` (the items of each `(f x)`, one after another) and
`(sort-by key coll)` (which make a vector), `(index-of coll x)` (the position
of the first item `=` to `x`, or `nil`), and `(some f coll)` (the first truthy `(f x)`, or `nil`) and `(every? f coll)` (whether
`(f x)` is truthy for all of them), which stop as soon as they know. All of
these take a vector, list or map; a map's items are its `[k v]` entries, each
a new pair (`doseq` with a `[k v]` pattern makes none). It also has the
struct macros below, and `defgc`.

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
(:a m)              ; => 1, a keyword called is get: (:z m 0) is (get m :z 0)
(put! m :c 3)       ; => {:a 1 :b 2 :c 3}, adds or replaces and returns m
(del! m :a)         ; => {:b 2 :c 3}, removes and returns m
(keys m)            ; => [:b :c]
(vals m)            ; => [2 3]
(count m)           ; => 2
(nth m 0)           ; => [:b 2], the entry at that place in insertion order
(= {:a 1 :b 2} {:b 2 :a 1}) ; => true, compared by contents
```

`doseq` walks a map's entries in order. With a `[k v]` pattern it makes no
pairs, so it's the cheap way to walk a map (cheaper than `(keys m)`, which
makes a vector):

```clojure
(doseq [[k v] m] ...)   ; Each key and value
(doseq [[k] m] ...)     ; Just the keys
(doseq [e m] ...)       ; Each entry as a new [k v]
```

Changing values while walking a map is fine, but don't add or remove keys:
added ones aren't visited, and removing one skips an entry and ends the walk
with an index error.

Any value can be a key. Vectors and maps are compared by contents, so don't
change one after using it as a key.

Vectors and maps inside a quoted value, like `'[1 2]`, are read-only. A quoted
value is a single constant shared by every evaluation, so changing one panics.

`assoc`, `dissoc` and `merge` leave their arguments alone and return a new
copy, which is never read-only:

```clojure
(def o {:a 1 :b 2})
(assoc o :a 9 :c 3)     ; => {:a 9 :b 2 :c 3}, a copy with those set (o is unchanged)
(assoc nil :x 1)        ; => {:x 1}
(assoc (Rect 3 4) :w 5) ; => #Rect{:w 5 :h 4}, a struct's fields too
(assoc [1 2 3] 0 :a 3 4); => [:a 2 3 4], a vector's indexes (len appends)
(dissoc o :a)           ; => {:b 2}, a copy without those keys
(merge {:a 1} {:b 2} {:a 3}) ; => {:a 3 :b 2}, later maps winning, nil skipped
```

Each copies the whole collection (a map without deleted keys with two
`memcpy`s), so they suit small maps like specs and options. To build up a big
one, `put!` into it.

## Structs, methods and interfaces

A struct has a fixed set of fields. Methods are defined outside it, and an
interface is a set of methods that any type with all of them satisfies,
without saying so, like Go.

```clojure
(defstruct Rect [w h])
(defstruct Circle [r])

(def a (Rect 3 4))      ; A value for each field, in order
(get a :w)              ; => 3
(:w a)                  ; => 3, the same
(put! a :w 10)          ; => #Rect{:w 10 :h 4}
(update! a :w + 1)      ; => #Rect{:w 11 :h 4}, (put! a :w (+ (get a :w) 1))
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

A keyword works anywhere a function does, as `get`: `(map :w rects)`.
`(update! x k f args...)` sets field (or map key) `k` to `(f (get x k)
args...)` and returns `x`; `x` and `k` are evaluated once. Both compile to the
same instructions as the `get` and `put!` they stand for.

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

## Finalizers

A struct type can have a finalizer, like Lua's `__gc`: a function that's
called with each of its structs once nothing can reach it, to free what it
holds, such as a texture or a file, without the program having to.

```clojure
(defstruct Voice [channel])
(defgc [v Voice] (stop-channel (get v :channel)))  ; A C binding

(defn play [sound]
  (Voice (start-channel sound)))
; Once nothing holds a Voice, a later collection finds it and the finalizer
; stops its channel
```

`defgc` is `(set-gc! Voice (fn [v] ...))`, and `(set-gc! Voice nil)` takes
the finalizer away. The finalizer can be a C function, so bindings can make
handles that free themselves, such as a game's images and meshes. As in Lua, only
structs made after their type gets a finalizer are finalized, so give it one
right after `defstruct`.

* A finalizer runs once per struct. If it stores the struct somewhere, the
  struct lives on, but it's never finalized again.
* What a struct holds stays alive until its finalizer has run, so the
  finalizer can use it. Its memory is freed by the collection after that.
* Collections happen during allocation, where running snap code isn't safe,
  so finalizers run soon after at a safe point: when a C function (like
  `gc-step`) returns, and between top-level forms. Those found by the same
  collection run newest first. A loop that allocates without calling any C
  function holds them back until it does.
* One that panics prints a warning to stderr, and the rest still run.
* When the program ends (`snap_destroy()`), every struct that still has a
  finalizer gets it run, live or not, as Lua does when it closes. A program
  that frees GPU resources in finalizers should call `snap_destroy()` before
  closing its window.

In C, `snap_set_gc(snap, type, fn)` sets one, and `snap_run_finalizers()`
runs any that are waiting.

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

A panic, or a compile error, remembers where it happened, and a script that
stops prints it, innermost first:

```
<err :snap_type_error "lib/world.snap: Expected numbers">
  at lib/world.snap:12 in grid-at (inlined)
  at lib/world.snap:40 in fill-grid
  at main.snap:7
```

`(err-where e)` gives the same as `[[file line name] ...]`, or `nil` for an
error that was only returned. `file` is `nil` for code compiled from a string
and `name` is `nil` for top-level code. A small function compiled into its
caller (see Implementation) has `:inlined` and the line its code starts on;
the line in its caller is the call's. Code a macro made has the line of the
macro call. An error raised in snap code that a C function called keeps its
places when the C function passes it on, and the trace stops at the
`recover` that caught it, at most 16 places. In C, `snap_err_where()` gives
the same vector and `snap_print_err()` prints an error with its places.

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
Errors don't undo them. Inside the core, `snap_anchor()` and `snap_release()`
do the short-lived version of this: they keep an object alive while C is
still building it (filling a new map allocates), and they come undone, last
first, by the end of the call or when an error unwinds.

## Reading arguments in C

A C function can take snap data the way snap code builds it, such as maps,
structs and vectors of `[x z]` pairs, through `SnapArgs`:

```c
static bool road_len(Snap* snap, const SValue* args, int num_args, SValue* result) {
  SnapArgs a;
  SVec* pts;
  float xz[2];
  double scale;
  int i;
  if (!snap_args(&a, snap, args, num_args, result, "(road-len road [scale])")) return false;
  pts = snap_vec(&a, snap_get(&a, snap_arg(&a, 0), "pts")); // (:pts road), a map or struct
  scale = snap_num_or(&a, snap_arg(&a, 1), 1.0);           // 1.0 if left out or nil
  for (i = 0; i < pts->len; ++i) snap_nums(&a, snap_at(&a, pts, i), xz, 2);
  if (a.failed) return false; // The first error is in *result
  ...
}
```

The usage string sets the arity: a plain name (or `<name>`, which can hold
spaces) is required, `[name]` is optional and `name...` takes the rest. Its
arity is cached by its address, so it should be a literal. It also names
what was wrong in the error, with the way there, as in `(road-len road
[scale]): road, :pts, item 3 should be a vector of at most 2 numbers, not a
vector`. The first error sticks. After it, reads
give safe zero values (0, `""`, an empty vector, or NULL from `snap_handle`),
so a function can read everything and check `a.failed` once, before using a
handle or changing anything.

- Accessors: `snap_arg`, `snap_get` (a field of a map or struct; nil has none)
  and `snap_at`. Each gives undef for something missing.
- Converters: `snap_num` (an int or float), `snap_int`, `snap_truthy`,
  `snap_name` (a string, keyword or symbol), `snap_vec`, `snap_handle` (a
  struct of a given type) and `snap_nums` (a vector of numbers into a float
  array). The `_or` forms of `snap_num` and `snap_int` give a default for
  something missing or nil.
- `snap_args_fail` fails with a message of the function's own.

Reading doesn't allocate, so pointers it gives stay valid without pins. A
field read is a string hash and a map lookup, which is fine for a function's
inputs; a loop run thousands of times should copy them into C arrays first.

## Implementation

The core is split by job, each `.c` with a header of the same name for what
the other files use:

| File | What's in it |
|---|---|
| `snap.c` | `snap_init()`, running code and defining globals: the API in `snap.h` |
| `snap_object.c` | Values, hashing, objects (maps, vectors, lists, structs) and printing |
| `snap_gc.c` | The collector, finalizers, pins and the allocation profile |
| `snap_vm.c` | The bytecode, frames, upvals and the interpreter |
| `snap_parse.c` | The reader (the lexer is in `snap_lex.rh`) |
| `snap_compile.c` | The compiler and the disassembler |
| `snap_builtins.c` | The built-in functions |

Embedders include only `snap.h`. The internal headers also hold, as `static
inline`, the helpers the interpreter, allocator and collector need inlined,
such as `gc_new()`, `map_get()` and `cached_field()`, so splitting the core
costs no calls. `make libsnap.a` builds the core for embedding,
and `main.c` runs a file or the REPL. The REPL is `repl.snap`, which the Makefile turns into a C
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
  is one instruction that carries the name. Calls to `nth`, `count`, `get`,
  `int`, `min` and `max` have instructions of their own that index vectors,
  look up maps and struct fields, truncate floats and compare numbers
  themselves, as long as the globals still hold the built-ins (otherwise, and
  for other arguments, they call the global). An expression whose value isn't
  used (all but the last in a body) is compiled without its value, so an `if`
  or `when` there doesn't push `nil` for a missing branch only to pop it. The
  condition of an `if` (so of `when` and `cond` too) compiles to jumps: `and`
  and `or` jump out at the operand that decides them, `not` (and `not=`)
  flips where the jumps go, and each compare takes its branch itself rather than pushing a
  bool. A condition that's a constant, like `cond`'s final `true`, is never
  tested, and the branch that can't run is dropped. Arithmetic and compares
  of two number constants, including `defconst` values like `(* PI 2.0)`,
  are worked out when compiling (except `/` and `%`, so dividing by zero is
  still an error when it runs).
* **The VM** is a stack machine with computed-goto dispatch, each handler
  ending in its own jump. Its loop keeps only what nearly every handler uses
  in local variables (pc, the stack, locals, constants and the `Snap`), few
  enough for the callee-saved registers, and each slow path (errors, calls to
  anything but snap code and C functions, `recover`, making closures) is a
  function it calls with its frame saved. Otherwise the compiler's register
  allocation is fragile: one more opcode can tip it into keeping pc in
  memory, which costs about 30%. It's compiled with `-O2`, where GCC and
  clang give the same code however many opcodes there are (GCC's `-O3`
  doesn't), and `make vmcheck` checks it. Once a function
  is compiled, each binary op is switched to a variant for its kinds of
  operands (stack, local or constant), which handles ints without decoding
  them and falls back to the general handler otherwise (for floats, without
  fetching the operands again). Counting loops end in
  a single `STEP` instruction that adds to the counter, compares it and jumps
  back. A closed-over variable that's never assigned (no `set!` of it) is
  copied into the closure, so making one costs one allocation. Others are
  shared between closures through an upvalue, closed when their frame
  returns. The compiler finds the `set!`s by looking through the function
  first. If a macro makes one after a closure copied the variable, the forms
  are compiled again, with that name shared.
* **Inline loops:** `some`, `every?`, `map` (of one collection), `filter`,
  `reduce`, `keep`, `remove`, `map-indexed` and `mapcat` called with a literal `fn` run the loop in place: no closure is
  made and the fn isn't called, so `(some (fn [x] (= x k)) v)` is about as
  fast as a hand-written `loop`. When it runs, a check falls back to the call
  as written if the collection isn't a vector or map, or the global no longer
  holds the stdlib function. The fn mustn't take `& rest` or use `recur`.
* **Inline functions:** a call to a global that holds a small function (at
  most 8 parameters, 40 values of source and 40 instructions, no `& rest`,
  closed-over variables, `fn`, `recur`, `def`, `defmacro` or `try`) compiles
  its body in place. A `GUARD_GLOBAL` instruction checks the global still
  holds that function and otherwise jumps to the call as written, so
  redefining it still works. The arguments go into new locals, except that a
  literal, or a local the caller never assigns, is used where it is; if the
  body assigns or closes over such a parameter, the caller is compiled again
  without that shortcut. While the body compiles the caller's locals are
  hidden, so its names mean what they mean in the function. Inlining goes 2
  deep and never into a function's own body, and a function keeps its own
  name in profiles. It makes `(defn dot [a b] ...)`-style helpers in numeric
  loops about as fast as writing them out.
* **Error locations:** while a form is compiled, the reader keeps a table
  from each list it read to its line, and each function keeps runs of
  instructions from the same line (and the same inlined functions), about 16
  bytes a line. A panic saves its frame's instruction, which calls already
  do, and makes a vector of each frame's code and instruction; lines and
  names are looked up only when they're asked for. So it costs nothing until
  something panics, then about 25ns more than before. Compiling is about 3%
  slower.
* **Peephole:** an instruction that makes a value (`nth`, `count`, `get`,
  `int`, `min`, `max`, a C call) followed by a store to a local writes the
  local itself and skips the store, and a jump to a jump goes straight to
  where the second one goes.
* **The stack** is one array for every frame, which grows (by reallocating
  and fixing the pointers into it) up to `SNAP_MAX_STACK`. It only moves when
  a function is called, so the VM reloads its pointers after a C function
  only if that C function ran snap code through `snap_call()`, which sets a
  flag, as Lua 5.4 does.
* **The GC** is incremental mark and sweep, so pauses stay short however big
  the heap is. A collection starts once the heap has grown to twice what was
  live after the last one, then runs in small steps that allocation pays for.
  Allocating 4KB does a step of about 2,000 units, and a step also stops
  after 15µs (looking at the clock every 256 units), carrying what's left
  into the next one, since units cost more when what they touch isn't in the
  cache. It carries at most 8 steps' worth, so the collector can't fall
  behind. `(gc-step n)` isn't timed. Marking uses a snapshot-at-the-beginning write barrier: while
  marking, a value removed from the heap (by `set!`, `put!`, `pop!` or
  `del!`) is marked first, so changes to the heap can't hide a live object.
  The stack is marked all at once when a collection starts, so stack writes,
  which are most of what the VM does, don't need the barrier. Objects made
  during a collection survive it. Big vectors and maps are marked 64 values at
  a time, and sweeping goes a few objects at a time. Which mark value means
  "marked" flips each collection, so objects never need to be unmarked.
* **Finalizers** work as in Lua 5.4. A struct whose type has a finalizer
  goes on a list when it's made, which isn't a root. When marking has reached
  everything, the structs on it that weren't marked move to a second list,
  which is a root, and get marked, along with what they reach; marking then
  carries on before the sweep. The VM runs the second list's finalizers after
  its next C call, through the check it already makes for C functions that
  ran snap code, so it costs nothing when there aren't any. Moving them goes
  through the first list 256 structs a step. Once marking has reached
  everything nothing can reach an unmarked struct, so that's safe, and they're
  only marked once all have moved, so a struct only reachable from another
  dying one is finalized in the same collection, as in Lua.
* **Allocation** takes constant time for objects up to 256 bytes (nearly
  all of them): they come from free lists of slots of 8 sizes, carved from
  16KB pages that are kept for reuse. Bigger objects get a page of their own.
  Sweeping walks the pages slot by slot, so objects need no list pointer and
  their header is 8 bytes. A vector made with up to 28 values keeps them in
  its own slot. Big objects (such as long strings), maps' arrays, bigger
  vectors' arrays and vectors' segments still come from `malloc`, and
  sweeping counts each `free()` as 8 units, so a step that frees many of them
  takes about as long as any other.
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
