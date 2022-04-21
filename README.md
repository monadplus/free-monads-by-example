# Free Monads: examples

This repository holds the sources from the series of blog post about free monads:
- [Free monads in the real world](https://monadplus.pro/haskell/2022/04/19/free-interpreter/)
- ...

## Building the project

Cabal v2: 

```sh
cabal update && cabal build
```

Stack:

```sh
stack build
```

Nix:

```sh
# Executable at ./result/bin/
nix-build
```

Development environment:

```sh
nix-shell --pure
```

## Running the project

Cabal v2:

```sh
cabal run
```

Stack:

```sh
stack run
```

Nix:

```sh
nix-build && ./result/bin/free-monads-examples
```

## CI

Disabled for now.

Nix+cachix is missing in this old template.
