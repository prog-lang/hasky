#include "machine.h"

Machine *Machine_new() {
  Machine *machine = malloc(sizeof(Machine));
  machine->stack = Stack_new();
  machine->sweeper = Sweeper_new();
  return machine;
}

void Machine_push(Machine *machine, Value *value) {
  Sweeper_may_collect_garbage(machine->sweeper, machine->stack);
  Sweeper_track(machine->sweeper, value);
  Stack_push(machine->stack, value);
}

Value *Machine_pop(Machine *machine) {
  Value *value = Stack_pop(machine->stack);
  return value;
}
