#ifndef PARSER_SYMTAB_H
#define PARSER_SYMTAB_H

#include <cstddef>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "ast.h"
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

  std::shared_ptr<Type> add_struct(const Type& tk, StructDeclPtr struct_node) {
    std::shared_ptr<Type> type = std::make_shared<Type>(tk);
    m_types.push_back(type);
    m_structs.insert({type, struct_node});
    return m_types.back();
  }

  std::shared_ptr<Type> add_func(const Type& tk, FuncDeclPtr func_node) {
    std::shared_ptr<Type> type = std::make_shared<Type>(tk);
    m_types.push_back(type);
    m_funcs.insert({type, func_node});
    return m_types.back();
  }

  const std::vector<std::shared_ptr<Type>>& get_types() const {
    return m_types;
  }
  const std::vector<std::shared_ptr<Variable>>& get_variables() const {
    return m_variables;
  }

  std::shared_ptr<Type> lookup_type(const Type& target) const;
  std::shared_ptr<Variable> lookup_variable(const std::string& name) const;

  StructDeclPtr lookup_struct(std::shared_ptr<Type> type) const {
    auto itr = m_structs.find(type);
    return itr == m_structs.end() ? nullptr : itr->second;
  }

  FuncDeclPtr lookup_func(std::shared_ptr<Type> type) const {
    auto itr = m_funcs.find(type);
    return itr == m_funcs.end() ? nullptr : itr->second;
  }

  void print(std::ostream& out, const std::string& indent = "") const;

private:
  size_t m_parent_scope; // parent scope id within symbol table
  std::vector<std::shared_ptr<Type>>
      m_types; // types declared within this scope
  std::vector<std::shared_ptr<Variable>>
      m_variables; // variables declared within this scope

  std::unordered_map<std::shared_ptr<Type>, StructDeclPtr> m_structs;
  std::unordered_map<std::shared_ptr<Type>, FuncDeclPtr> m_funcs;
};

class SymTab {
public:
  SymTab();

  void enter_new_scope();
  void exit_scope();

  std::shared_ptr<Type> lookup_type(const Type& target) const;

  std::shared_ptr<Variable> lookup_variable(const std::string& name,
                                            int starting_scope = -1) const;

  StructDeclPtr lookup_struct(std::shared_ptr<Type> type) const;
  FuncDeclPtr lookup_func(std::shared_ptr<Type> type) const;

  std::shared_ptr<Type> get_primitive_type(std::string primitive) const;

  std::shared_ptr<Type> declare_type(const Type& tk);
  std::shared_ptr<Variable> declare_variable(Variable v);

  std::shared_ptr<Type> declare_struct(const Type& tk,
                                       StructDeclPtr struct_node);
  std::shared_ptr<Type> declare_func(const Type& tk, FuncDeclPtr func_node);

  size_t current_scope() const { return m_current_scope; }

  void print(std::ostream& out) const;

private:
  size_t m_current_scope;      // current scope id
  std::vector<Scope> m_scopes; // v[scope id] <-> Scope
};

#endif // PARSER_SYMTAB_H
