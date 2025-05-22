#ifndef PARSER_TYPES_H
#define PARSER_TYPES_H

#include <memory>
#include <string>
#include <variant>
#include <vector>

struct Variable;

// == Type System ==
struct NamedType;
struct FunctionType;

using TypeKind =
    std::variant<std::unique_ptr<NamedType>, std::unique_ptr<FunctionType>>;

// == Valid Type Kinds: ==
// Note: Do not consider scope when checking equality. Scope per type is
// essentially only useful for the lsp to find Type declarations. When comparing
// for TypeKinds during lookup, we will already be searching in the
// valid/accessible scopes, and if we included a check for equal scopes, it
// woudl not consider the inheritence of types from parent scopes.

// structs and primitives
struct NamedType {
  std::string identifier;
  size_t scope_id;
  NamedType(const std::string& id, size_t sc) : identifier(id), scope_id(sc) {}
};

struct FunctionType {
  std::vector<Variable> parameters;
  TypeKind return_type;
  size_t scope_id;

  // Variables must be moved due to holding TypeKinds
  FunctionType(std::vector<Variable> params, TypeKind ret_type, size_t sc)
      : parameters(std::move(params)),
        return_type(std::move(ret_type)),
        scope_id(sc) {}
};

// == Variables ==
enum BorrowState {
  MutablyOwned,      // as param: owned mut
  ImmutableOwned,    // as param: owned
  MutablyBorrowed,   // as param: mut
  ImmutablyBorrowed, // as param: read
};

struct Variable {
  Variable(const std::string& name, BorrowState mod, TypeKind tk, size_t sc)
      : name(name), modifier(mod), type(std::move(tk)), scope_id(sc) {}

  std::string name;
  BorrowState modifier;
  TypeKind type;
  size_t scope_id;
};

#endif // PARSER_TYPES_H
