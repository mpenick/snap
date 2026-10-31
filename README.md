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
`+ - * / %` and `< <= > >= =`. `(- x)` negates, and `(+ x)` is `x` (it must be a number).

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
* Predicates: `nil?`, `vector?`, `map?`, `list?` and `sym?`
* Errors: `err`, `err?`, `err-kind` and `err-msg`
* Lists: `list`, `cons`, `first`, `rest`, `next` (like `rest`, but `nil`
  instead of `()` at the end), `nthnext` (the values from an index on, of a
  list or vector, or `nil`), `concat` (of lists and vectors)
  and `vec` (a list to a vector). `count` and `nth` work on lists too. `nil`
  counts as an empty list.
* Macros: `gensym` and `macroexpand` (see below)
* Types: `type`, `make-type`, `add-method!`, `find-method` and `set-gc!`
  (see Structs and Finalizers below)
* Vectors and maps: `count`, `nth`, `push!`, `pop!`, `put!`, `get`, `del!`,
  `keys` and `vals`
* Strings: `(str x...)` joins strings, and other values as they print
* Math: `sqrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `exp`, `log`, `(pow x y)`
  and `(atan2 y x)` give floats; `floor`, `ceil`, `round` (half away from
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
`defn`, `defconst`, `when`, `cond`, `if-let`, `when-let`, `dotimes`,
`(doseq [x coll] ...)` (over a vector, list or map), `inc`, `dec`, `map`,
`filter`, `reduce` and `(sort-by key coll)` (which take a vector or list and
make a vector), the
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
handles that free themselves, as quake's images and meshes do. As in Lua, only
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

## Doom

`doom/` is a small doom-like game, written to stress the language: a
Wolfenstein-style raycaster that draws every pixel of a 320x200 framebuffer
from snap (textured walls, floors and ceilings, and sprites, with textures it
makes when it starts), demons that chase you, and pickups. It runs at about
200 frames a second, with two thirds of the time in the VM. The corner of the screen
shows the GC's stats: it allocates a few KB a frame, and its pauses stay
around 100 microseconds.

![doom](doom/doom.png)

It uses [raylib](https://www.raylib.com), which isn't packaged on Ubuntu, so
`make raylib` in `doom/` fetches it and builds it there (this needs the X11
and GL headers, e.g. `libx11-dev`, `libxrandr-dev`, `libxinerama-dev`,
`libxcursor-dev`, `libxi-dev` and `libgl-dev`). `doom/snap_game.c` has the
bindings (windows, input, drawing and images) and a `main()` that runs a file
with them. Images are `Image` structs with a C finalizer (see Finalizers), so
the game never frees them. Finding an image's slot from its struct checks
that the struct owns it, and the last two found are cached, as drawing goes
back and forth between the framebuffer and a texture for every pixel.

```
cd doom
make raylib   # Once
make run      # WASD, mouse or arrows to turn, click or space to shoot, tab for the map
```

## Quake

`quake/` is a small arena shooter in the style of quake 3, the next stress
test after doom. The GPU draws it, so the work in snap is everywhere else. When
it starts, it makes its textures and meshes, lights every vertex of the arena
(sun, sky and colored lights, each casting a shadow ray), and finds which
places bots can walk between by trying each walk with the game's physics.
While it plays, snap runs quake 3's movement for everyone (strafe jumping,
rocket jumping, stairs and jump pads), the rockets and rails, items, and four
bots. The bots find paths (Dijkstra), go after items, and aim, strafe and hop
at you and at each other. With all five players being bots, it runs 600
frames in about 0.6 seconds.

![quake](quake/quake.png)

It shares doom's raylib build. `quake/snap_game.c` is doom's with 3D added:
a camera, cubes, spheres and cylinders, blending, and meshes built a vertex
at a time. Its images and meshes are `Image` and `Mesh` structs with C
finalizers (see Finalizers), so the game never frees them: their pixels,
triangles and GPU textures and buffers go when they're dropped, or when the
program ends, before the window closes. If the tables run out of slots,
making one runs a whole collection and the finalizers first.

```
cd quake
make run      # WASD and mouse, space to jump, click to shoot, 1-3 for weapons, tab for scores
```

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
  is one instruction that carries the name. Calls to `nth`, `count`, `get`,
  `int`, `min` and `max` have instructions of their own that index vectors,
  look up maps and struct fields, truncate floats and compare numbers
  themselves, as long as the globals still hold the built-ins (otherwise, and
  for other arguments, they call the global). An expression whose value isn't
  used (all but the last in a body) is compiled without its value, so an `if`
  or `when` there doesn't push `nil` for a missing branch only to pop it. The
  condition of an `if` (so of `when` and `cond` too) compiles to jumps: `and`
  and `or` jump out at the operand that decides them, `not` flips where the
  jumps go, and each compare takes its branch itself rather than pushing a
  bool. A condition that's a constant, like `cond`'s final `true`, is never
  tested, and the branch that can't run is dropped. Arithmetic and compares
  of two number constants, including `defconst` values like `(* PI 2.0)`,
  are worked out when compiling (except `/` and `%`, so dividing by zero is
  still an error when it runs).
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
* **Finalizers** work as in Lua 5.4. A struct whose type has a finalizer
  goes on a list when it's made, which isn't a root. When marking has reached
  everything, the structs on it that weren't marked move to a second list,
  which is a root, and get marked, along with what they reach; marking then
  carries on before the sweep. The VM runs the second list's finalizers after
  its next C call, through the check it already makes for C functions that
  ran snap code, so it costs nothing when there aren't any. Moving them goes
  through the first list in one step, rather than incrementally.
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
