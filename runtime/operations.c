#include "operations.h"

Value *add(ValueClosure *closure) {
  ValueInt *x = (ValueInt *)(closure->args[0]);
  ValueInt *y = (ValueInt *)(closure->args[1]);
  return (Value *)Int_new(x->value + y->value);
}
