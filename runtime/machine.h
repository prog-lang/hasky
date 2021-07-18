#pragma once

#include "stack.h"
#include "types.h"

#define INITIAL_GC_THRESHOLD 20

typedef struct {
  Stack *stack;

  /* GC stuff. */
  Value *head;
  size_t allocated;
  size_t threshold;
} Machine;

Machine *Machine_new();
void Machine_push(Machine *machine, Value *value);
Value *Machine_pop(Machine *machine);
