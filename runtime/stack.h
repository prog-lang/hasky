#pragma once

#include "assert.h"
#include "config.h"
#include "types.h"
#include <stdlib.h>

typedef struct {
  Value *items[MAX_STACK_SIZE];
  size_t size;
} Stack;

Stack *Stack_new();
void Stack_push(Stack *stack, Value *value);
Value *Stack_pop(Stack *stack);
