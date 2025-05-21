#ifndef PARSER_VARIABLE_H
#define PARSER_VARIABLE_H

#include <string>
#include <vector>

#include "types.h"

enum BorrowState {
  MutablyOwned,
  ImmutableOwned,
  MutablyBorrowed,
  ImmutablyBorrowed,
};

struct Variable {
  Variable(const std::string& name, BorrowState mod, TypeKind tk, size_t sc)
      : name(name), modifier(mod), type(std::move(tk)), scope_id(sc) {}

  std::string name;
  BorrowState modifier;
  TypeKind type;
  size_t scope_id;
};

#endif // PARSER_VARIABLE_H
