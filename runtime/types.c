#include "types.h"

void default_mark(Value *value) { value->reached = true; }
void default_free(Value *value) { free(value); }

ValueByte *Byte_new(uint8_t byte) {
  ValueByte *value = malloc(sizeof(ValueByte));
  value->type = TypeInt;
  value->mark = default_mark;
  value->free = default_free;
  value->value = byte;
  return value;
}

ValueInt *Int_new(uint32_t integer) {
  ValueInt *value = malloc(sizeof(ValueInt));
  value->type = TypeInt;
  value->mark = default_mark;
  value->free = default_free;
  value->value = integer;
  return value;
}

ValueTagged *Tagged_new(const char *tag, void *tagged) {
  ValueTagged *value = malloc(sizeof(ValueTagged));
  value->type = TypeTagged;
  value->mark = default_mark;
  value->free = default_free;
  value->tag = tag;
  value->value = tagged;
  return value;
}

void Closure_mark(Value *value) {
  ValueClosure *lambda = (ValueClosure *)value;
  lambda->reached = true;
  for (size_t i = 0; i < lambda->argc; i++)
    lambda->args[i]->reached = true;
}

void Closure_free(Value *value) {
  ValueClosure *lambda = (ValueClosure *)value;
  for (size_t i = 0; i < lambda->argc; i++) {
    Value *arg = lambda->args[i];
    if (!arg->reached)
      arg->free(arg);
  }
  free(lambda);
}

ValueClosure *Closure_apply(ValueClosure *src, Value *arg) {
  assert(src->argi < src->argc, "Argument overflow in closure!");
  ValueClosure *dest = malloc(sizeof(ValueClosure));
  memcpy(dest, src, sizeof(ValueClosure));
  dest->args[dest->argi++] = arg;
  return dest;
}

ValueClosure *Closure_new(const size_t argc, Value *(*exec)(ValueClosure *)) {
  ValueClosure *value = malloc(sizeof(ValueClosure));
  value->type = TypeClosure;
  value->mark = Closure_mark;
  value->free = Closure_free;
  value->argi = 0;
  value->argc = argc;
  value->args = calloc(argc, sizeof(Value *));
  value->apply = Closure_apply;
  value->exec = exec;
  return value;
}
