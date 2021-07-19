#include "operations.h"

void add(Stack *stack) {
  ValueInt *x = (ValueInt *)Stack_pop(stack);
  ValueInt *y = (ValueInt *)Stack_pop(stack);
  Stack_push(stack, (Value *)Int_new(x->value + y->value));
}
