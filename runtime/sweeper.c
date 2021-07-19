#include "sweeper.h"
#include "stdbool.h"

Sweeper *Sweeper_new(size_t threshold) {
  Sweeper *sweeper = malloc(sizeof(Sweeper));
  sweeper->head = NULL;
  sweeper->allocated = 0;
  sweeper->threshold = threshold;
  return sweeper;
}

void Sweeper_track(Sweeper *sweeper, Value *value) {
  value->reached = false;
  value->next = sweeper->head;
  sweeper->head = value;
  sweeper->allocated++;
}

void Sweeper_mark(Sweeper *sweeper, Stack *stack) {
  for (size_t i = 0; i < stack->size; i++) {
    Value *value = stack->items[i];
    value->mark(value);
  }
}

/* Sweeper linked list of allocated values:
 * [1] -> [2] -> [3] -> [4] -> [5] -> NULL
 *  ^ head
 */
void Sweeper_sweep(Sweeper *sweeper) {
  Value **value = &sweeper->head;
  while (*value) {
    if ((*value)->reached) {
      (*value)->reached = false;
      value = &(*value)->next;
      continue;
    }
    Value *unreachable = *value;
    *value = unreachable->next;
    unreachable->free(unreachable);
    sweeper->allocated--;
  }
}

void Sweeper_collect_garbage(Sweeper *sweeper, Stack *stack) {
  Sweeper_mark(sweeper, stack);
  Sweeper_sweep(sweeper);
  sweeper->threshold =
      sweeper->allocated ? sweeper->allocated * 2 : INITIAL_GC_THRESHOLD;
}

void Sweeper_may_collect_garbage(Sweeper *sweeper, Stack *stack) {
  if (sweeper->allocated >= sweeper->threshold)
    Sweeper_collect_garbage(sweeper, stack);
}
