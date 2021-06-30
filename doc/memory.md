# Memory

Hasky is a pure functional language and does not allow mutation! Therefore,
functions can receive arguments as references (pointers) to the underlying
types. This won't be a problem for garbage collection as those references
are guaranteed not to be mutated.

All data has explicit owner (function/scope). As soon as variable goes out of
scope, it will be deallocated unless it is returned from a function. This is
**static** garbage collection, like in Rust, but it doesn't require lifetime
specifiers.

## Example 1

```hs
def main := println (+ 2 40);
```

The above program has 3 function applications:

- `+ 2`
- `(+ 2) 40`
- `println (+ 2 40)`

Let's see roughly what happens at the hardware level:

```c
void main(void) {
    Box     a = Box_new(2);
    Box     b = Box_new(42);
    Lambda2 c = Lambda_new(Int_add, 2);
    Lambda2 d = Lambda_app(c, a);
    Lambda2 e = Lambda_app(d, b);
    Box     f = Lambda_exe();
    Lambda  g = Lambda_new(Int_println, 1);
    Lambda  h = Lambda_app(h, f);
                Lambda_exe();
}
```

## Example 2

```hs
def pair := Pair 2 40;
def sum  := \p . + (fst p) (snd p);
def main := sum pair;
```

And compiled:

```c
Wrap2 pair(void) {
    Box   a = Box_new(2);
    Box   b = Box_new(42);
    Wrap2 c = Wrap_new(Pair);
    Wrap2 d = Wrap_add(a);
    Wrap2 e = Wrap_add(b);
    return e;
}
```
