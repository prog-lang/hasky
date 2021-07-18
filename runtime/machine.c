#include "machine.h"
#include <stdio.h>

Machine *Machine_new() {
  Machine *machine = malloc(sizeof(Machine));
  machine->stack = Stack_new();
  machine->head = NULL;
  machine->allocated = 0;
  machine->threshold = INITIAL_GC_THRESHOLD;
  return machine;
}

void Machine_mark(Machine *machine) {
  for (size_t i = 0; i < machine->stack->size; i++) {
    Value *item = machine->stack->items[i];
    item->mark(item);
  }
}

void Machine_sweep(Machine *machine) {
  Value **head = &machine->head;
  while (*head) {
    if ((*head)->reached) { // this Value has been marked and should be kept
      (*head)->reached = true;
      head = &(*head)->next;
      continue;
    }

    Value *unreached = *head; // this Value is unreachable and must be freed
    head = &unreached->next;
    unreached->free(unreached);
    machine->allocated--;
  }
}

void Machine_collect_garbage(Machine *machine) {
  size_t beforeSweep = machine->allocated;
  Machine_mark(machine);
  Machine_sweep(machine);
  machine->threshold =
      machine->allocated == 0 ? INITIAL_GC_THRESHOLD : machine->allocated * 2;
  printf("Swept %ld, left %ld\n", beforeSweep - machine->allocated,
         machine->allocated);
  printf("[%ld] %ld/%ld\n", machine->stack->size, machine->allocated,
         machine->threshold);
}

void Machine_push(Machine *machine, Value *value) {
  printf("Pusing ");
  Value_print(value);
  printf("\n");

  if (machine->allocated >= machine->threshold)
    Machine_collect_garbage(machine);
  value->reached = false;
  value->next = machine->head;
  machine->head = value;
  Stack_push(machine->stack, value);
  machine->allocated++;
  printf("[%ld] %ld/%ld\n", machine->stack->size, machine->allocated,
         machine->threshold);
}

Value *Machine_pop(Machine *machine) {
  Value *value = Stack_pop(machine->stack);
  printf("Poping ");
  Value_print(value);
  printf("\n");
  printf("[%ld] %ld/%ld\n", machine->stack->size, machine->allocated,
         machine->threshold);
  return value;
}
