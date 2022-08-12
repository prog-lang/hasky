-- Builtin primitives.
mod core;

type List a := Nil | Cons a (List a);

pub def ++ := \xs. \ys.
  when xs is
    Nil then ys,
    Cons hd tl then Cons hd (++ tl ys);

type Int;

pub def + :- Int . Int . Int;
pub def - :- Int . Int . Int;
pub def * :- Int . Int . Int;
pub def / :- Int . Int . Int;

type Byte;
type alias Str := List Byte;
