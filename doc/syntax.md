# Syntax

## Module

```
-- Module name declaration.
mod lib;

-- Import statements (aliased if needed).
use core:io as io;

-- Module-level definitions with no type signature required!
pub def magic := 42;

-- Definitions are all shaped like `name := expression;` which simplifies
-- parsing and compilation.
pub def first := \x. \y. x;

pub def greet := \x. io:println (++ "hello, " x);
```

## Main

```
-- Executable programs are compiled based on an entrypoint file that has to
-- contain the `main` function and must not have a `mod` declaration.

use core:io as io;
use lib;

def main := do
  io:println "Resistance is futile! Your actions don't matter...";
  let name := io:input "Enter your name: "
  in lib:greet (lib:first lib:magic name);
end
```
