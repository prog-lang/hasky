#include "../lib/machine.h"
#include "_main_.h"

int main(void) {
  Machine *machine = Machine_new();
  _main_(machine);
  return 0;
}
