#include "operations.h"
#include "types.h"

void add(Stack *stack) {
  Stack_push(stack,
             Int_new(Stack_pop(stack)->integer + Stack_pop(stack)->integer));
}
