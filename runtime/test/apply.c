/*
 * Make sure that argi is incremented in the new Closure after Closure_apply.
 *
 * We *do not* care about the fact that all Closure instances share the same
 * memory for their args (so long as there's no concurrency involved) because
 * only one instance will be executing at a time.
 *
 * This is virtually equivalent to using Closure's args as its private
 * registers. Yet, we must ensure that we disallow the following code pattern
 * in compiled code:
 *
 *     ValueClosure *a = Closure_new(2, add);              // args: [ *, * ]
 *     ValueClosure *b = a->apply(a, (Value *)Int_new(1)); // args: [ 1, * ]
 *     ValueClosure *c = a->apply(a, (Value *)Int_new(2)); // args: [ 2, * ]
 *     ValueClosure *d = b->apply(b, (Value *)Int_new(3)); // args: [ 2, 3 ]
 *     Value   *result = d->exec(d);                       // result: 5
 *
 * The result should have been 4 (instead of 5), but c decided to apply 2 to a
 * before b had a chance to finish its applications and execution.
 *
 * This pitfall can be avoided by creating additional Closures or, my preferred
 * way, by ensuring that each operation is finished before another operation
 * begins using the Closure again. Essentially, Closure operations must be
 * *atomic*, and if it turns out that they are not, we will have to also clone
 * the args memory.
 *
 * Additional optimisations could be performed:
 *
 *     1. Compile time function execution and expression evaluation (e.g. Zig).
 *     2. Closure unwind onto the Stack in cases where it's not taken by a
 *        higher-order function as a parameter.
 *
 */

#include "../lib/operations.h"
#include "../lib/types.h"

int main(void) {
  ValueClosure *a = Closure_new(2, add);
  assert(!a->argi, "argi of a new Closure must be 0");
  ValueClosure *b = a->apply(a, (Value *)Int_new(1));
  assert(a->argi + 1 == b->argi, "argi alteration failure a->b");
  ValueClosure *c = b->apply(b, (Value *)Int_new(2));
  assert(b->argi + 1 == c->argi, "argi alteration failure b->c");
  return 0;
}
