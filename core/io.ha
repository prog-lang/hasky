-- Standard input/output functions.
mod core:io;

-- print doesn't have a definition as it's defined within the RTS.
pub def print :- Str . ();

-- println is defined in terms of print.
pub def println := \s. print (++ s "\n");

