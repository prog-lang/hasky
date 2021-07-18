#include "stack.h"

Stack *Stack_new() {
  Stack *stack = malloc(sizeof(Stack));
  return stack;
}

void Stack_push(Stack *stack, Value *value) {
  assert(stack->size <= MAX_STACK_SIZE, "Stack overflow caused a crash!");
  stack->items[stack->size++] = value;
}

Value *Stack_pop(Stack *stack) {
  assert(stack->size > 0, "Stack underflow caused a crash!");
  Value *popped = stack->items[--stack->size];
  return popped;
}
