#ifndef PARSER_TYPES_H
#define PARSER_TYPES_H

#include <memory>
#include <string>
#include <variant>
#include <vector>

// forward declaration of type kinds
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

  friend bool operator==(const NamedType& lhs, const NamedType& rhs) {
    return lhs.identifier == rhs.identifier;
  }
};

enum ParameterModifier {
  Owned,
  ReadRef,
  MutableRef,
};

struct FunctionParamType {
  ParameterModifier modifier = ReadRef;
  TypeKind type;
  size_t scope_id;

  FunctionParamType(TypeKind param_type, size_t sc)
      : type(std::move(param_type)), scope_id(sc) {}

  friend bool operator==(const FunctionParamType& lhs,
                         const FunctionParamType& rhs) {
    // ignore modifier, this will not be a part of the method family
    return lhs.type == rhs.type;
  }
};

struct FunctionType {
  std::vector<FunctionParamType> parameters;
  TypeKind return_type;
  size_t scope_id;

  FunctionType(const std::vector<FunctionParamType>& params, TypeKind ret_type,
               size_t sc)
      : parameters(params), return_type(std::move(ret_type)), scope_id(sc) {}

  friend bool operator==(const FunctionType& lhs, const FunctionType& rhs) {
    return lhs.parameters == rhs.parameters &&
           lhs.return_type == rhs.return_type;
  }
};

#endif // PARSER_TYPES_H
