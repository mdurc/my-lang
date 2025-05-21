#include "symtab.h"

bool operator==(const TypeKind& a, const TypeKind& b) {
  if (a.index() != b.index()) return false;
  return std::visit([&a, &b]() { return a == b; });
}

const TypeKind* Scope::lookup_type(const TypeKind& target) const {
  for (const TypeKind& type : m_types) {
    if (type == target) {
      return &type;
    }
  }
  return nullptr;
}

const Variable* Scope::lookup_variable(const std::string& target_name) const {
  for (const Variable& var : m_variables) {
    if (var.name == target_name) {
      return &var;
    }
  }
  return nullptr;
}

SymTab::SymTab() {
  m_current_scope = 0;
  m_scopes.emplace_back(-1); // global scope

  // define the primitives in the language
  m_scopes[0].add_type(std::make_unique<NamedType>("u0", 0));
  m_scopes[0].add_type(std::make_unique<NamedType>("u8", 0));
  m_scopes[0].add_type(std::make_unique<NamedType>("u16", 0));
  m_scopes[0].add_type(std::make_unique<NamedType>("u32", 0));
  m_scopes[0].add_type(std::make_unique<NamedType>("u64", 0));
  m_scopes[0].add_type(std::make_unique<NamedType>("i8", 0));
  m_scopes[0].add_type(std::make_unique<NamedType>("i16", 0));
  m_scopes[0].add_type(std::make_unique<NamedType>("i32", 0));
  m_scopes[0].add_type(std::make_unique<NamedType>("i64", 0));
  m_scopes[0].add_type(std::make_unique<NamedType>("f64", 0));
  m_scopes[0].add_type(std::make_unique<NamedType>("bool", 0));
  m_scopes[0].add_type(std::make_unique<NamedType>("string", 0));
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

const TypeKind* SymTab::lookup_type(const TypeKind& target) const {
  const Scope* scope = &m_scopes[m_current_scope];
  while (true) {
    const TypeKind* tk = scope->lookup_type(target);
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

const Variable* SymTab::lookup_variable(const std::string& target_name) const {
  const Scope* scope = &m_scopes[m_current_scope];
  while (true) {
    const Variable* v = scope->lookup_variable(target_name);
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

void SymTab::declare_type(TypeKind tk) {
  m_scopes[m_current_scope].add_type(std::move(tk));
}

void SymTab::declare_variable(Variable v) {
  m_scopes[m_current_scope].add_variable(std::move(v));
}
