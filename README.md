# Parley

Functional language inspired by [Haskell](https://www.haskell.org/) and [LISP](https://en.wikipedia.org/wiki/Lisp_(programming_language)).

Long term plan is to produce frontend to [LLVM](https://llvm.org/) but only when language semantics and syntax have been settled down.

## Status

Language is still very much in progress, see [TODO](doc/todo.md) for more details.

## Syntax

See [syntax](doc/syntax.md) for more details, some valid programs in [examples](examples/).

## Requirements

- [Stack](https://docs.haskellstack.org/en/stable/README/) build system for Haskell
- GHC Haskell 8.X.X
- LLVM 6.0

## Tests

Run tests
```
$ stack test
```

Get test coverage
```
$ stack test --coverage
...
The coverage report for parley's test-suite "parley-test" is available at /home/developer/repos/parley/.stack-work/install/x86_64-linux/lts-12.8/8.4.3/hpc/parley/parley-test/hpc_index.html
```
Open coverage report with your browser.


## Running

Build with

```
$ stack build
```

Parse parley file with
```
$ stack exec parley-exe ./examples/fib.par
```

Currently program parses one example and outputs:
- code itself
- pretty printed parse tree
- pretty printed abstract syntax tree with type information for that scope

Example output:
```
Code:
main = stdin: Integer -> fib stdin
  
  fib = n: 1 -> id 0
  fib = n: 2 -> id 1
  fib = n: Integer -> plus f1 f2
    n1 <- minus n 1
    n2 <- minus n 2
    f1 <- fib n1
    f2 <- fib n2
Parse tree:
stdout <- main stdin
main = stdin: Integer -> fib stdin
  fib = n: 1 -> id 0
  fib = n: 2 -> id 1
  fib = n: Integer -> plus f1 f2
    n1 <- minus n 1
    n2 <- minus n 2
    f1 <- fib n1
    f2 <- fib n2

AST:
  stdout <- main stdin
    # main: Function(Integer, Integer)
    # stdin: Integer
    # stdout: Integer
  
    main = stdin: Integer -> fib stdin
      # fib: Function(Integer, Integer)
      # main: Function(Integer, Integer)
      # stdin: Integer
    
      fib = n: 1 -> id 0
        # fib: Function(1, Integer)
        # id: Function(t, t)
        # n: 1
    
      fib = n: 2 -> id 1
        # fib: Function(2, Integer)
        # id: Function(t, t)
        # n: 2
    
      fib = n: Integer -> plus f1 f2
        # f1: Integer
        # f2: Integer
        # fib: Function(Integer, Integer)
        # n: Integer
        # plus: Function(Integer, Integer, Integer)
      
        n1 <- minus n 1
          # minus: Function(Integer, Integer, Integer)
          # n: Integer
          # n1: Integer
      
        n2 <- minus n 2
          # minus: Function(Integer, Integer, Integer)
          # n: Integer
          # n2: Integer
      
        f1 <- fib n1
          # f1: Integer
          # fib: Function(Integer, Integer)
          # n1: Integer
      
        f2 <- fib n2
          # f2: Integer
          # fib: Function(Integer, Integer)
          # n2: Integer

```