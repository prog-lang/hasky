#pragma once

#include <stdint.h>

typedef uint8_t Byte;
typedef int32_t Int;
typedef double Float;

typedef struct {
  void *head;
  void *tail;
} Pair;
