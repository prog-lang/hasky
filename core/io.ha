-- Standard input/output functions.
mod core:io;

pub def readup :- Byte . Str;

pub def readn :- Int . Str;

pub def readln := readup '\n';

pub def print :- Str . ();

pub def println := \s. print (++ s "\n");

