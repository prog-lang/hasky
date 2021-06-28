/* Generated Hasky program. */

#pragma once

#include "../ops.h"
#include "../types.h"
#include "env.h"
#include <stdio.h>

/* main := + 2 40 |> println */
void hasky_main(void) {
  Env2 e1 = Env_new;     // +
  Int i1 = 2;            // 2
  Env_app(e1, i1);       // + 2
  Int i2 = 40;           // 40
  Env_app(e1, i2);       // + 2 40
  Int i3 = Int_add(&e1); // 42
  println(Int_show(i3)); // println 42
}
