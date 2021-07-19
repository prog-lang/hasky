#pragma once

#include "stack.h"
#include "types.h"

#define INITIAL_GC_THRESHOLD 20

typedef struct {
  Value *head;
  size_t allocated;
  size_t threshold;
} Sweeper;

Sweeper *Sweeper_new();
void Sweeper_track(Sweeper *sweeper, Value *value);
void Sweeper_mark(Sweeper *sweeper, Stack *stack);
void Sweeper_sweep(Sweeper *sweeper);
void Sweeper_collect_garbage(Sweeper *sweeper, Stack *stack);
void Sweeper_may_collect_garbage(Sweeper *sweeper, Stack *stack);
