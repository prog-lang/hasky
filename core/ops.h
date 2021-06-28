#pragma once

#include "gen/env.h"
#include "types.h"
#include <stdint.h>
#include <stdio.h>

Int Int_add(Env2 *env) {
  Int x = Env_get(Int, 0);
  Int y = Env_get(Int, 1);
  return x + y;
}

char *Int_show(Int i) {
  char *buf = (char *)malloc(sizeof(char) * 15);
  sprintf(buf, "%d", i);
  return buf;
}

void println(char *str) { printf("%s\n", str); }
