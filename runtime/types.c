#include "types.h"

void default_mark(Value *value) { value->reached = true; }
void default_free(Value *value) { free(value); }

Value *Value_print(Value *value) {
  switch (value->type) {
  case TypeByte:
    printf("%c", ((ValueByte *)value)->value);
    break;

  case TypeInt:
    printf("%d", ((ValueInt *)value)->value);
    break;

  default:
    printf("Unknown Value Type: %d\n", value->type);
    kaboom("Attempt to print unknown Value Type");
  }
  return value;
}

ValueByte *Byte_new(uint8_t byte) {
  ValueByte *value = malloc(sizeof(ValueByte));
  value->type = TypeInt;
  value->value = byte;
  value->mark = default_mark;
  value->free = default_free;
  return value;
}

ValueInt *Int_new(uint32_t integer) {
  ValueInt *value = malloc(sizeof(ValueInt));
  value->type = TypeInt;
  value->value = integer;
  value->mark = default_mark;
  value->free = default_free;
  return value;
}

ValueTagged *Tagged_new(const char *tag, void *tagged) {
  ValueTagged *value = malloc(sizeof(ValueTagged));
  value->type = TypeTagged;
  value->tag = tag;
  value->value = tagged;
  value->mark = default_mark;
  value->free = default_free;
  return value;
}
