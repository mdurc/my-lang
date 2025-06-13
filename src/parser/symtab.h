#ifndef PARSER_SYMTAB_H
#define PARSER_SYMTAB_H

#include <cstddef>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "types.h"

class Scope {
public:
  Scope(size_t ps) : m_parent_scope(ps) {}

  size_t get_parent_scope() const { return m_parent_scope; }

  std::shared_ptr<Type> add_type(const Type& tk) {
    m_types.push_back(std::make_shared<Type>(tk));
    return m_types.back();
  }
  std::shared_ptr<Variable> add_variable(const Variable& v) {
    m_variables.push_back(std::make_shared<Variable>(v));
    return m_variables.back();
  }

  const std::vector<std::shared_ptr<Type>>& get_types() const {
    return m_types;
  }
  const std::vector<std::shared_ptr<Variable>>& get_variables() const {
    return m_variables;
  }

  std::shared_ptr<Type> lookup_type(const Type& target) const;
  std::shared_ptr<Variable> lookup_variable(const std::string& name) const;

  void print(std::ostream& out, const std::string& indent = "") const;

private:
  size_t m_parent_scope; // parent scope id within symbol table
  std::vector<std::shared_ptr<Type>>
      m_types; // types declared within this scope
  std::vector<std::shared_ptr<Variable>>
      m_variables; // variables declared within this scope
};

class SymTab {
public:
  SymTab();

  void enter_new_scope();
  void exit_scope();

  std::shared_ptr<Type> lookup_type(const Type& target) const;

  std::shared_ptr<Variable> lookup_variable(const std::string& name,
                                            int starting_scope = -1) const;

  std::shared_ptr<Type> get_primitive_type(std::string primitive) const;

  std::shared_ptr<Type> declare_type(const Type& tk);
  std::shared_ptr<Variable> declare_variable(Variable v);

  size_t current_scope() const { return m_current_scope; }

  void print(std::ostream& out) const;

private:
  size_t m_current_scope;      // current scope id
  std::vector<Scope> m_scopes; // v[scope id] <-> Scope
};

#endif // PARSER_SYMTAB_H
