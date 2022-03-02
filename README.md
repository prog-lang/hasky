# Hasky Programming Language

Before Haskell became a strict and boring adult, he was a bad boy nicknamed
Hasky by his street bros...

## Installation

### Hasky Compiler

Depends on `stack` or `cabal`.

```bash
cd compiler/
stack install # or `cabal install`
```

> See [compiler/README.md](compiler/README.md) for compiler usage instructions!

### Hasky Tool

Depends on `go` and `hasky-compile` to be installed and available from `$PATH`.

```bash
go install ./hasky/
```

> Hasky Tool calls `hasky-compile` if you invoke it on `*.ha` files, therefore,
> you won't have to use `hasky-compile` manually as a regular Hasky user.

## Usage Tutorial

Given a simple source file named `main.ha`:

```rs
mod main;

main := 42;
```

Run the following:

```bash
hasky main.ha  # => out.ha.bin (executable bytecode)
./main.ha.bin  # execute the program
```
