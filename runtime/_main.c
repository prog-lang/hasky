#include "_main_.h"
#include "machine.h"

int main(void) {
  Machine *machine = Machine_new();
  _main_(machine);
  return 0;
}
