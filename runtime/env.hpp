#pragma once

#include <stdlib.h>

#define Env(ac)                                                                \
  struct {                                                                     \
    void *env[ac];                                                             \
    size_t argc;                                                               \
  }

#define Env_new                                                                \
  { {}, 0 }

#define Env_app(e, arg) e.env[e.argc++] = (void *)&arg

#define Env_get(type, id) *(type *)env->env[id]
