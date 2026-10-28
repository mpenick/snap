# Snap

A weird Clojure-ish lisp that I built to write garbage collectors for and to
sort of learn functional programming. The goal is a very small VM and GC that
are fast, small and simple.

## Features

* Closures
* Tail recursion (`recur` and `loop`)
* Byte code compiler and stack-based interpreter
* Mark-and-sweep GC
* FFI (C functions)
* Errors as values (no exceptions)

## Building and running

Building needs a C compiler and [Ragel](https://www.colm.net/open-source/ragel/)
(for the lexer).

```
make
./snap [-d] <file> <arg_1>...<arg_n>
```

`-d` prints the byte code before running it. The script's arguments are a list
in `args`.

```
make test     # Run tests/ and compare with the expected output
make stress   # The same under ASan/UBSan, collecting every few bytes
```

## Language

```clojure
(def fib (fn [n]
  (loop [[a 0] [b 1] [i 0]]
    (if (= i n) a (recur b (+ a b) (+ i 1))))))

(println (fib 50))
```

Special forms: `def`, `set!`, `fn`, `let`, `loop`, `recur`, `if`, `begin`,
`quote`, `and`, `or`, `not`, `try`, `panic`, and the operators `+ - * / %` and
`< <= > >= =`.

Values are 63-bit integers, floats, strings, keywords (`:name`), symbols,
lists, `nil`, `true`, `false`, functions and errors. Square brackets are only
used for parameters and bindings. `nil` and `false` are false, everything else
is true. Integer arithmetic wraps on overflow, and mixing integers with floats
gives a float.

Built-in functions: `print`, `println`, `nil?`, `err`, `err?`, `err-kind` and
`err-msg`.

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
  (let [[x (try (safe-div a b))]]
    (+ x (try (safe-div x c))))))

(calc 100 10 5)   ; => 12
(calc 100 0 5)    ; => <err :div-by-zero "can't divide by zero">

; Handle one kind and pass the rest up
(def div-or-zero (fn [a b]
  (let [[r (safe-div a b)]]
    (if (err? r :div-by-zero) 0 (try r)))))
```

`(panic kind msg)` is for bugs. It can't be caught and stops the program, as
do type errors, division by zero and calls with the wrong number of arguments.
An error that `try` returns from the top level of a script also stops it.

C functions return `false` to panic, with `*result` set to the error. To
return an error as a value, set `*result` to it and return `true`.

## Implementation

Everything is in `snap.c` (lexer in `snap_lex.rh`, `main.c` runs a file).

* **Values** are 8 bytes. The low bits tag integers, objects, `nil`/booleans
  and C functions, so integers need no allocation. Floats are heap objects.
* **The compiler** turns each form straight into byte code, one 32-bit
  instruction at a time, with a few peepholes. Binary operators can read
  locals and constants directly and store arithmetic results straight into a
  local, so most of a tight loop doesn't touch the stack.
* **The VM** is a stack machine with computed-goto dispatch. Closed-over
  variables are shared between closures and closed when their frame returns.
* **The GC** is mark and sweep with an explicit mark stack. It collects once
  the heap has grown to twice what was live after the last collection.

## TODO

* Better GC
