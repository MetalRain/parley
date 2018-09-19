# Parley

Functional language inspired by [Haskell](https://www.haskell.org/) and [LISP](https://en.wikipedia.org/wiki/Lisp_(programming_language)). Tries to make out of order (and parallel) execution more likely by giving less guarantees about order of execution.

Long term plan is to produce frontend to [LLVM](https://llvm.org/) but only when language semantics and syntax have been settled down.

## Status

Language is still very much in progress, there is only incomplete parser and many things like iteration or conditionals are not though out or implemented.

See [TODO](doc/todo.md) for more details.

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

