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

  void add_type(TypeKind tk) { m_types.push_back(std::move(tk)); }
  void add_variable(Variable v) { m_variables.push_back(std::move(v)); }

  const std::vector<TypeKind>& get_types() const { return m_types; }
  const std::vector<Variable>& get_variables() const { return m_variables; }

  const TypeKind* lookup_type(const TypeKind& target) const;
  const Variable* lookup_variable(const std::string& target_name) const;

private:
  size_t m_parent_scope;             // parent scope id within symbol table
  std::vector<TypeKind> m_types;     // types declared within this scope
  std::vector<Variable> m_variables; // variables declared within this scope
};

class SymTab {
public:
  SymTab();

  void enter_new_scope();
  void exit_scope();

  const TypeKind* lookup_type(const TypeKind& target) const;
  const Variable* lookup_variable(const std::string& target_name) const;

  const TypeKind* get_primitive_type(std::string primitive) const;

  void declare_type(TypeKind tk);
  void declare_variable(Variable v);

  size_t current_scope() const { return m_current_scope; }

private:
  size_t m_current_scope;      // current scope id
  std::vector<Scope> m_scopes; // v[scope id] <-> Scope
};

#endif // PARSER_SYMTAB_H
