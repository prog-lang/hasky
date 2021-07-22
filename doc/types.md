# Types

## Basic Data Types

- Byte
- Int
- Float
- Lambda

## Algebraic Data Types

Algebraic data types can be constructed in Hasky using the following syntax:

```hs
type Bool := True | False;
```

## Types Defined in Core

Basic utility data types:

```hs
type Bool := True | False;
type Pair a b := Pair a b;
type List a := Head a (List a) | Nil;
```

Result types:

```hs
type Maybe a := Some a | None;
type Result a b := Err a | Ok b;
type Either a b := Left a | Right b;
```
