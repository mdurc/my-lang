#include "symtab.h"

std::shared_ptr<Type> Scope::lookup_type(const Type& target) const {
  for (std::shared_ptr<Type> type : m_types) {
    if (*type == target) {
      return type;
    }
  }
  return nullptr;
}

std::shared_ptr<Variable> Scope::lookup_variable(
    const std::string& name) const {
  for (std::shared_ptr<Variable> var : m_variables) {
    if (var->name == name) {
      return var;
    }
  }
  return nullptr;
}

SymTab::SymTab() {
  m_current_scope = 0;
  m_scopes.emplace_back(-1); // global scope

  // define the primitives in the language
  m_scopes[0].add_type(Type(Type::Named("u0"), 0));
  m_scopes[0].add_type(Type(Type::Named("u8"), 0));
  m_scopes[0].add_type(Type(Type::Named("u16"), 0));
  m_scopes[0].add_type(Type(Type::Named("u32"), 0));
  m_scopes[0].add_type(Type(Type::Named("u64"), 0));
  m_scopes[0].add_type(Type(Type::Named("i8"), 0));
  m_scopes[0].add_type(Type(Type::Named("i16"), 0));
  m_scopes[0].add_type(Type(Type::Named("i32"), 0));
  m_scopes[0].add_type(Type(Type::Named("i64"), 0));
  m_scopes[0].add_type(Type(Type::Named("f64"), 0));
  m_scopes[0].add_type(Type(Type::Named("bool"), 0));
  m_scopes[0].add_type(Type(Type::Named("string"), 0));
}

void SymTab::enter_new_scope() {
  m_scopes.emplace_back(m_current_scope);
  m_current_scope = m_scopes.size() - 1;
}

void SymTab::exit_scope() {
  if (m_current_scope != 0) {
    m_current_scope = m_scopes[m_current_scope].get_parent_scope();
  }
}

std::shared_ptr<Type> SymTab::lookup_type(const Type& target) const {
  const Scope* scope = &m_scopes[m_current_scope];
  while (true) {
    std::shared_ptr<Type> tk = scope->lookup_type(target);
    if (tk != nullptr) {
      return tk;
    }
    int next_id = scope->get_parent_scope();
    if (next_id == -1) {
      break;
    }
    scope = &m_scopes[next_id];
  }
  return nullptr;
}

std::shared_ptr<Variable> SymTab::lookup_variable(
    const std::string& target_name) const {
  const Scope* scope = &m_scopes[m_current_scope];
  while (true) {
    std::shared_ptr<Variable> v = scope->lookup_variable(target_name);
    if (v != nullptr) {
      return v;
    }
    int next_id = scope->get_parent_scope();
    if (next_id == -1) {
      break;
    }
    scope = &m_scopes[next_id];
  }
  return nullptr;
}

std::shared_ptr<Type> SymTab::get_primitive_type(std::string primitive) const {
  return m_scopes[0].lookup_type(Type(Type::Named(std::move(primitive)), 0));
}

std::shared_ptr<Type> SymTab::declare_type(Type tk) {
  if (m_scopes[m_current_scope].lookup_type(tk)) {
    return nullptr;
  }
  return m_scopes[m_current_scope].add_type(tk);
}

std::shared_ptr<Variable> SymTab::declare_variable(Variable v) {
  if (m_scopes[m_current_scope].lookup_variable(v.name)) {
    return nullptr;
  }
  return m_scopes[m_current_scope].add_variable(std::move(v));
}
