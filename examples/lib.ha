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

