#include "types.h"

// Comparator functor for TypeKinds
struct TypeKindComparator {
  template <typename T, typename U>
  bool operator()(const std::shared_ptr<T>& a,
                  const std::shared_ptr<U>& b) const {
    if constexpr (std::is_same_v<T, U>) {
      return *a == *b;
    } else {
      return false;
    }
  }
};

bool operator==(const TypeKind& a, const TypeKind& b) {
  if (a.index() != b.index()) return false;
  return std::visit(TypeKindComparator{}, a, b);
}

bool operator==(const NamedType& a, const NamedType& b) {
  return a.identifier == b.identifier;
}

bool operator==(const FunctionType& a, const FunctionType& b) {
  return a.parameters == b.parameters && a.return_type == b.return_type;
}

bool operator==(const PointerType& a, const PointerType& b) {
  return a.is_mutable == b.is_mutable && a.pointee == b.pointee;
}

bool operator==(const Variable& a, const Variable& b) {
  return a.name == b.name;
}
