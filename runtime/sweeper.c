#include "sweeper.h"

Sweeper *Sweeper_new() {
  Sweeper *sweeper = malloc(sizeof(Sweeper));
  sweeper->head = NULL;
  sweeper->allocated = 0;
  sweeper->threshold = INITIAL_GC_THRESHOLD;
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
  size_t beforeGC = sweeper->allocated;
  Sweeper_mark(sweeper, stack);
  Sweeper_sweep(sweeper);
  sweeper->threshold =
      sweeper->allocated ? sweeper->allocated * 2 : INITIAL_GC_THRESHOLD;
  if (DEBUG)
    printf("SWEPT %ld, KEPT %ld, NEW THRESHOLD %ld\n",
           beforeGC - sweeper->allocated, sweeper->allocated,
           sweeper->threshold);
}

void Sweeper_may_collect_garbage(Sweeper *sweeper, Stack *stack) {
  if (sweeper->allocated >= sweeper->threshold)
    Sweeper_collect_garbage(sweeper, stack);
}
