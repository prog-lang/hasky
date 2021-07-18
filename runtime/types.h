#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef enum {
  TypeByte,
  TypeInt,
  TypeTagged,
} Type;

typedef struct sValue {
  Type type;
  union {
    uint8_t byte;
    uint32_t integer;
    struct {
      const char *tag;
      void *value;
    };
  };

  /* GC stuff. */
  bool reached;                  // bool flag to mark reachable Values
  struct sValue *next;           // next allocated Value
  void (*mark)(struct sValue *); // mark knows how to mark this Value
  void (*free)(struct sValue *); // free knows how to free this Value
} Value;

Value *Value_new();
Value *Value_print(Value *value);
Value *Byte_new(uint8_t byte);
Value *Int_new(uint32_t integer);
