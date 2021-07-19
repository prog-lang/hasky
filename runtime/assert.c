#include "assert.h"

void assert(bool condition, char *message) {
  if (!condition)
    kaboom(message);
}
