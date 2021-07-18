#include "types.h"
#include <stdio.h>

void default_mark(Value *value) { value->reached = true; }
void default_free(Value *value) { free(value); }

Value *Value_new(Type type) {
  Value *value = malloc(sizeof(Value));
  value->type = type;
  return value;
}

Value *Value_print(Value *value) {
  switch (value->type) {
  case TypeInt:
    printf("#%d", value->integer);
    break;
  default:
    printf("non-int");
  }
  return value;
}

Value *Byte_new(uint8_t byte) {
  Value *value = Value_new(TypeByte);
  value->byte = byte;
  value->mark = default_mark;
  value->free = default_free;
  return value;
}

Value *Int_new(uint32_t integer) {
  Value *value = Value_new(TypeInt);
  value->integer = integer;
  value->mark = default_mark;
  value->free = default_free;
  return value;
}
