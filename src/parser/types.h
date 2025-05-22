#ifndef PARSER_TYPES_H
#define PARSER_TYPES_H

#include <memory>
#include <string>
#include <variant>
#include <vector>

// == Type System ==

enum BorrowState {
  MutablyOwned,      // as param: owned mut
  ImmutableOwned,    // as param: owned
  MutablyBorrowed,   // as param: mut
  ImmutablyBorrowed, // as param: read
};

class Type {
public:
  struct Named {
    std::string identifier;
    Named(std::string id) : identifier(std::move(id)) {}

    friend bool operator==(const Named& a, const Named& b) {
      return a.identifier == b.identifier;
    }
  };

  struct Function {
    std::vector<std::pair<BorrowState, std::shared_ptr<Type>>> parameters;
    std::shared_ptr<Type> return_type;

    Function(std::vector<std::pair<BorrowState, std::shared_ptr<Type>>> params,
             std::shared_ptr<Type> ret_type)
        : parameters(std::move(params)), return_type(ret_type) {}

    friend bool operator==(const Function& a, const Function& b) {
      return a.parameters == b.parameters && a.return_type == b.return_type;
    }
  };

  struct Pointer {
    bool is_mutable;
    std::shared_ptr<Type> pointee;

    Pointer(bool is_mutable, std::shared_ptr<Type> pointee)
        : is_mutable(is_mutable), pointee(pointee) {}

    friend bool operator==(const Pointer& a, const Pointer& b) {
      return a.is_mutable == b.is_mutable && a.pointee == b.pointee;
    }
  };

  // Public interface
  template <typename T>
  bool is() const {
    return std::holds_alternative<T>(storage);
  }
  template <typename T>
  const T& as() const {
    return std::get<T>(storage);
  }

  // Constructors
  Type(Named n, size_t sc) : storage(std::move(n)), scope_id(sc) {}
  Type(Function f, size_t sc) : storage(std::move(f)), scope_id(sc) {}
  Type(Pointer p, size_t sc) : storage(std::move(p)), scope_id(sc) {}

  friend bool operator==(const Type& a, const Type& b) {
    return a.storage == b.storage;
  }

private:
  std::variant<Named, Function, Pointer> storage;
  size_t scope_id;
};

// == Variables ==
class Variable {
public:
  std::string name;
  BorrowState modifier;
  std::shared_ptr<Type> type;
  size_t scope_id;

  Variable(std::string name, BorrowState mod, std::shared_ptr<Type> tk,
           size_t sc)
      : name(std::move(name)), modifier(mod), type(tk), scope_id(sc) {}

  friend bool operator==(const Variable& a, const Variable& b) {
    return a.name == b.name;
  }
};

#endif // PARSER_TYPES_H
