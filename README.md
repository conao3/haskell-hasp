# hasp

A minimal Lisp interpreter written in Haskell. The name combines "Haskell" and "Lisp" to reflect its dual heritage.

## Overview

hasp implements a small but functional subset of Lisp with support for:

- Integer literals and symbols
- Conditionals with `if`
- Variable bindings with `def`
- First-class functions with `lambda`
- Quote expressions

## Getting Started

### Prerequisites

- GHC 9.4 or later
- Cabal 3.0 or later

### Building

```sh
cabal build
```

### Running

```sh
cabal run hasp
```

This starts an interactive REPL where you can evaluate Lisp expressions.

## Language Features

### Atoms

Integers and symbols are the basic building blocks:

```lisp
> 1
1
> 42
42
```

### Conditionals

The `if` form evaluates the first branch when the condition is non-zero, and the second branch when zero:

```lisp
> (if 1 2 3)
2
> (if 0 2 3)
3
```

### Variable Definitions

Use `def` to bind values to names:

```lisp
> (def a 1)
1
> a
1
```

### Lambda Functions

Create anonymous functions with `lambda`:

```lisp
> (lambda (a) (if a 2 3))
#<lambda:a>
> ((lambda (a) (if a 2 3)) 1)
2
> ((lambda (a) (if a 2 3)) 0)
3
```

Combine `def` and `lambda` to define named functions:

```lisp
> (def f (lambda (a) (if a 2 3)))
#<lambda:a>
> (f 1)
2
> (f 0)
3
```

## License

Apache-2.0
