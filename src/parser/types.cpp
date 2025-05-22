#include "types.h"

// Comparator functor for TypeKinds
struct TypeKindComparator {
  template <typename T, typename U>
  bool operator()(const std::unique_ptr<T>& lhs,
                  const std::unique_ptr<U>& rhs) const {
    if constexpr (std::is_same_v<T, U>) {
      return *lhs == *rhs;
    } else {
      return false;
    }
  }
};

bool operator==(const TypeKind& a, const TypeKind& b) {
  if (a.index() != b.index()) return false;
  return std::visit(TypeKindComparator{}, a, b);
}

bool operator==(const NamedType& lhs, const NamedType& rhs) {
  return lhs.identifier == rhs.identifier;
}

bool operator==(const FunctionType& lhs, const FunctionType& rhs) {
  return lhs.parameters == rhs.parameters && lhs.return_type == rhs.return_type;
}

bool operator==(const Variable& lhs, const Variable& rhs) {
  return lhs.name == rhs.name;
}
