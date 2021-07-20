#pragma once

#include "assert.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
  TypeByte,
  TypeInt,
  TypeTagged,
  TypeClosure,
} Type;

#define GCstuff                                                                \
  bool reached;                                                                \
  struct baseValue *next;                                                      \
  void (*mark)(struct baseValue *);                                            \
  void (*free)(struct baseValue *)

#define ValuePrelude                                                           \
  Type type;                                                                   \
  GCstuff

// Value is a base "class" that only contains common fields like Type and GC
// stuff. The ValuePrelude macro above encompasses those common fields.
//
// > The ValuePrelude must be inserted *at the very top* of each Value subclass.
//
// Any runtime value can be down-casted into Value because of that, which
// allows us to fake polymorphism.
typedef struct baseValue {
  ValuePrelude;
} Value;

typedef struct {
  ValuePrelude;
  uint32_t value;
} ValueInt;

typedef struct {
  ValuePrelude;
  uint8_t value;
} ValueByte;

typedef struct {
  ValuePrelude;
  const char *tag;
  void *value;
} ValueTagged;

typedef struct closureValue {
  ValuePrelude;
  size_t argi;  // arg index
  size_t argc;  // arg count
  Value **args; // array of pointers to Value where closure stores arguments
  void (*apply)(struct closureValue *, Value *);
  Value *(*exec)(struct closureValue *);
} ValueClosure;

ValueByte *Byte_new(uint8_t byte);
ValueInt *Int_new(uint32_t integer);
ValueTagged *Tagged_new(const char *tag, void *value);
ValueClosure *Closure_new(const size_t argc, Value *(*exec)(ValueClosure *));
