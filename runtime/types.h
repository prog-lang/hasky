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

typedef struct baseValue {
  Type type;
  /* GC stuff. */
  bool reached;                     // bool flag to mark reachable Values
  struct baseValue *next;           // next allocated Value
  void (*mark)(struct baseValue *); // mark knows how to mark this Value
  void (*free)(struct baseValue *); // free knows how to free this Value
} Value;

typedef struct {
  /* Mandatory prelude. */
  Type type;
  bool reached;
  Value *next;
  void (*mark)(Value *);
  void (*free)(Value *);

  uint32_t value;
} ValueInt;

typedef struct {
  /* Mandatory prelude. */
  Type type;
  bool reached;
  Value *next;
  void (*mark)(Value *);
  void (*free)(Value *);

  uint8_t value;
} ValueByte;

typedef struct {
  /* Mandatory prelude. */
  Type type;
  bool reached;
  Value *next;
  void (*mark)(Value *);
  void (*free)(Value *);

  const char *tag;
  void *value;
} ValueTagged;

typedef struct closureValue {
  /* Mandatory prelude. */
  Type type;
  bool reached;
  Value *next;
  void (*mark)(Value *);
  void (*free)(Value *);

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
