# Parley

Functional language inspired by [Haskell](https://www.haskell.org/), [LISP](https://en.wikipedia.org/wiki/Lisp_(programming_language)) and [SSA](https://en.wikipedia.org/wiki/Static_single_assignment_form). Focuses on making parallel execution simple & possible by using single assignment form and referential transparency.

Long term plan is to produce frontend to [LLVM](https://llvm.org/) to build robust compiler once language grammar and syntax have been found. 

## Status

Language is still very much in progress, not usable for any useful programs.

See [TODO](doc/todo.md) for more details.

## Syntax

WIP
See [syntax](doc/syntax.md).

Early prototypes of programs in [examples](doc/examples.md). Actual "programs" in [tests](test/Spec.hs).

## Requirements

- [Stack](https://docs.haskellstack.org/en/stable/README/) build system for Haskell
- GHC Haskell 8.X.X

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

