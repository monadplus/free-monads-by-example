# Free monads by example [![badge]][pipeline]

Companion code for ["Free monads in the real world"](https://monadplus.pro/haskell/2022/04/19/free-interpreter/).

## Build the project

```bash
# Option 1
$ nix-shell
nix-shell> cabal build

# Option 2
nix-build

# Option 3
stack build
```

The project also compiles with GHC `9.6.X` and cabal `3.X.X`, but prefer deterministic builds with nix.

[badge]: https://github.com/monadplus/free-monads-by-example/actions/workflows/ci.yml/badge.svg
[pipeline]: https://github.com/monadplus/free-monads-by-example/actions/workflows/ci.yml?query=branch%3Amaster
