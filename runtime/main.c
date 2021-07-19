#include "_main.h"
#include "machine.h"

int main(void) {
  Machine *machine = Machine_new();
  _main(machine);
  return 0;
}
