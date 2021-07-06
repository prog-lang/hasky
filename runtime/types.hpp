#pragma once

#include <stdint.h>

namespace core {

typedef uint8_t Byte;
typedef int32_t Int;

enum ListTag { Nil, Cons };
struct List_Byte {
  ListTag tag;

  struct {
    Byte head;
    List_Byte *tail;
  } cons;

  List_Byte() { this->tag = Nil; }

  List_Byte(const Byte b) {
    this->tag = Cons;
    this->cons = {b, nullptr};
  }

  List_Byte(const Byte *cs) {
    if (cs[0] == '\0') {
      this->tag = Nil;
      return;
    }
    this->tag = Cons;
    this->cons = {cs[0], new List_Byte(cs + 1)};
  }
};

} // namespace core
