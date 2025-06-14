#ifndef PARSER_TYPES_H
#define PARSER_TYPES_H

#include <cstdint>
#include <iostream>
#include <memory>
#include <ostream>
#include <string>
#include <variant>
#include <vector>

// == Type System ==

enum BorrowState {
  MutablyOwned,      // as param: take mut
  ImmutableOwned,    // as param: take | take imm
  MutablyBorrowed,   // as param: mut
  ImmutablyBorrowed, // as param: imm
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
    std::vector<std::shared_ptr<Type>> params;
    std::shared_ptr<Type> return_type;

    Function(std::vector<std::shared_ptr<Type>> params,
             std::shared_ptr<Type> ret_type)
        : params(std::move(params)), return_type(ret_type) {}

    friend bool operator==(const Function& a, const Function& b) {
      return a.params == b.params && a.return_type == b.return_type;
    }
  };

  struct Pointer {
    bool is_pointee_mutable;
    std::shared_ptr<Type> pointee;

    Pointer(bool is_pointee_mutable, std::shared_ptr<Type> pointee)
        : is_pointee_mutable(is_pointee_mutable), pointee(pointee) {}

    friend bool operator==(const Pointer& a, const Pointer& b) {
      return a.is_pointee_mutable == b.is_pointee_mutable &&
             a.pointee == b.pointee;
    }
  };

  // Public interface
  template <typename T>
  bool is() const {
    return std::holds_alternative<T>(m_storage);
  }
  template <typename T>
  const T& as() const {
    return std::get<T>(m_storage);
  }

  inline static const uint64_t PTR_SIZE = 8;

  // returns the string format that should be used within the language
  std::string to_string() const;
  size_t get_scope_id() const { return m_scope_id; }
  uint64_t get_byte_size() const { return m_bytes; }
  void set_byte_size(uint64_t size) { m_bytes = size; }

  // Constructors
  Type(Named n, size_t sc, uint64_t bytes = PTR_SIZE)
      : m_storage(std::move(n)), m_scope_id(sc), m_bytes(bytes) {}
  Type(Function f, size_t sc)
      : m_storage(std::move(f)), m_scope_id(sc), m_bytes(PTR_SIZE) {}
  Type(Pointer p, size_t sc)
      : m_storage(std::move(p)), m_scope_id(sc), m_bytes(PTR_SIZE) {}

  friend bool operator==(const Type& a, const Type& b) {
    return a.m_storage == b.m_storage;
  }

private:
  std::variant<Named, Function, Pointer> m_storage;
  size_t m_scope_id;
  uint64_t m_bytes;

  std::string to_string_recursive(std::vector<const Type*>& visited) const;
};

// == Variables ==
class Variable {
public:
  std::string name;
  BorrowState modifier;
  std::shared_ptr<Type> type; // nullptr means it must be inferred
  size_t scope_id;
  bool is_return_var;

  Variable(std::string name, BorrowState mod, std::shared_ptr<Type> tk,
           size_t sc, bool ret_var = false)
      : name(std::move(name)),
        modifier(mod),
        type(tk),
        scope_id(sc),
        is_return_var(ret_var) {}

  friend bool operator==(const Variable& a, const Variable& b) {
    return a.name == b.name;
  }
};

#endif // PARSER_TYPES_H
