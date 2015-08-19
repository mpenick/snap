# Code Generation

## Opcodes

POP

LOAD_GLOBAL index_of_name
STORE_GLOBAL index_of_name

LOAD_LOCAL index
STORE_LOCAL index

LOAD_CLOSED index
STORE_CLOSED index

LOAD_CONSTANT index

JUMP offset
JUMP_TRUE offset
JUMP_FALSE offset

LESS_THAN
LESS_EQUALS
GREATER_THAN
GREATER_EQUALS
EQUALS
NOT_EQUALS
NOT

ADD
SUB
MUL
DIV
MOD

CALL num_params
RETURN

CREATE_CLOSURE num_closed

### Forms

(fn (<params>...) <exprs....)
(define <symbol> <expr>)
(set! <symbol> <expr>
(recur <args>...)
(if <cond> <expr1> <expr2>)
(do <exprs>...)
(let (<bindings>...) <exprs>...)
(loop (<bindings>...) <exprs>...)
(quote <expr>)

#### Maybe
(try <expr> <catch>)
(throw <expr>)

## Examples

### `define` (global)

```lisp
(define a 1)
```

```
LOAD_CONSTANT 0 ; 1
STORE_GLOBAL 0  ; a
``

### `define` (fn)

```lisp
(define foo (fn (a b c) (+ a b c)))
(print (foo 1 2 3))
```

```
LOAD_LOCAL 0 ; a
LOAD_LOCAL 1 ; b
LOAD_LOCAL 2 ; c
ADD ; a + b
ADD ; r + c
RETURN
```

```
LOAD_CONSTANT 0 ; code object for fn
STORE_GLOBAL 0  ; foo
LOAD_CONSTANT 1 ; 1
LOAD_CONSTANT 2 ; 2
LOAD_CONSTANT 3 ; 3
LOAD_GLOBAL 0   ; foo
CALL 3
LOAD_GLOBAL 1   ; print
CALL 1
```

### Function call

```lisp
(foo a b c)

```
LOAD_GLOBAL 1 ; a
LOAD_GLOBAL 2 ; b
LOAD_GLOBAL 3 ; c
LOAD_GLOBAL 0 ; foo
CALL 3
```

### `let`

```lisp
(let ((a 1) (b 2))
  (+ a b)
)
```

```
LOAD_CONSTANT 0
STORE_LOCAL 0
LOAD_CONSTANT 1
STORE_LOCAL 1
LOAD_LOCAL 0
LOAD_LOCAL 1
ADD
```

```
LOAD_CONSTANT 0
LOAD_CONSTANT 1
ADD
```

### Tail recursion

```lisp
(def inner (fn (a s i m)
               (if (< i m) (recur a (add s (mul a i)) (add i 1) m) s)
               ))
(print (inner 1 0 0 10000))
```

```
LOAD_LOCAL 2     ; i, start of the loop
LOAD_LOCAL 3     ; m
LESS_THAN        ; (< i m)
JUMP_FALSE 17

LOAD_LOCAL 0     ; a

LOAD_LOCAL 1     ; s

LOAD_LOCAL 0     ; a
LOAD_LOCAL 2     ; i
MUL              ; a * i
ADD              ; s + a * i

LOAD_LOCAL 1    ; i
LOAD_CONSTANT 0 ; 1
ADD             ; i + 1

LOAD_LOCAL 3    ; m

STORE_LOCAL 3   ; m = m
STORE_LOCAL 2   ; i = i + 1
STORE_LOCAL 1   ; s += a * i
STORE_LOCAL 0   ; a = a
JUMP -18        ; loop
RETURN          ; dead code

LOAD_LOCAL 1    ; end condition
RETURN
```

```
LOAD_CONSTANT 2
LOAD_CONSTANT 3
LOAD_CONSTANT 3
LOAD_CONSTANT 4
LOAD_CONSTANT 1
CALL 4
LOAD_CONSTANT 0
CALL 1
```

### Closure

```lisp
(def inc (fn (x)
             (fn ()
                 (do
                   (set! x (add x 1))
                   x)
                 )
             ))

(def inc1 (inc 0))
(print (inc1))
(print (inc1))
```

```
LOAD_CLOSED 0   ; x
LOAD_CONSTANT 0 ; 1
ADD
STORE_CLOSED 0
LOAD_CLOSED 0
RETURN
```

```
LOAD_LOCAL 0     ; x
LOAD_CONSTANT 0  ; internal function
CREATE_CLOSURE 1
RETURN
```

```
LOAD_CONSTANT 1
LOAD_CONSTANT 0
CALL 1
STORE_GLOBAL 1 ; inc1

LOAD_GLOBAL  1 ; inc1
CALL 0
LOAD_GLOBAL  0 ; print
CALL 1

LOAD_GLOBAL  1 ; inc1
CALL 0
LOAD_GLOBAL  0 ; print
CALL 1
```

### Recursion

```lisp
(def fact (fn (n)
              (if (= n 1) 1
                (mul n (fact (sub n 1)))
                )
              ))
(print (fact 20))
```

```
LOAD_LOCAL 0    ; n
LOAD_CONSTANT 0 ; 1
EQUALS          ; n == 1
JUMP_FALSE 3

LOAD_CONSTANT 0 ; 1
RETURN

LOAD_LOCAL 0    ; n

LOAD_LOCAL 0    ; n
LOAD_CONSTANT 0 ; 1
SUB             ; n - 1
LOAD_GLOBAL 0   ; fact
CALL 1          ; fact(n -1)

MUL             ; n * fact(n - 1)
RETURN
```
