#pragma once

#include "stack.h"
#include "sweeper.h"
#include "types.h"
#include <stdio.h>

typedef struct {
  Stack *stack;
  Sweeper *sweeper;
} Machine;

Machine *Machine_new();
void Machine_push(Machine *machine, Value *value);
Value *Machine_pop(Machine *machine);
