#pragma once

#include "assert.h"
#include "types.h"
#include <stdlib.h>

#define MAX_STACK_SIZE 2048

typedef struct {
  Value *items[MAX_STACK_SIZE];
  size_t size;
} Stack;

Stack *Stack_new();
void Stack_push(Stack *stack, Value *value);
Value *Stack_pop(Stack *stack);
