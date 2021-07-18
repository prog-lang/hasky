#include "assert.h"

void assert(bool condition, const char *message) {
  if (!condition) {
    printf("%s\n", message);
    exit(1);
  }
}
