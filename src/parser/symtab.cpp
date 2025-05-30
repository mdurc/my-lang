#include "symtab.h"

// Helper for type output
static std::string borrowed_state_to_string(BorrowState bs) {
  switch (bs) {
    case BorrowState::MutablyOwned:
    case BorrowState::MutablyBorrowed: return "mut";
    case BorrowState::ImmutableOwned:
    case BorrowState::ImmutablyBorrowed: return "imm";
    default: return "";
  }
}

// == Scope Implementations ==
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

void Scope::print(std::ostream& out, const std::string& indent) const {
  out << indent << "Parent Scope ID: " << m_parent_scope << "\n";
  out << indent << "Declared Types (" << m_types.size() << "):\n";
  for (const std::shared_ptr<Type>& type : m_types) {
    out << indent << "  - "
        << (type ? type->to_string() : "err:null_type_was_found")
        << " (declared in scope "
        << (type ? std::to_string(type->get_scope_id()) : "?err?") << ")\n";
  }

  out << indent << "Declared Variables (" << m_variables.size() << "):\n";
  for (const std::shared_ptr<Variable>& var : m_variables) {
    out << indent << "  - Name: " << var->name << "\n";
    out << indent
        << "      Modifier: " << borrowed_state_to_string(var->modifier)
        << "\n";
    out << indent << "      Type: "
        << (var->type ? var->type->to_string() : "nullptr/inferred") << "\n";
    out << indent << "      Scope ID: " << var->scope_id << "\n";
  }
}

// == Symbol Table Implementations ==
SymTab::SymTab() {
  m_current_scope = 0;
  m_scopes.emplace_back(0); // global scope

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
  size_t scope_id = target.get_scope_id();
  const Scope* scope = &m_scopes[scope_id];
  while (true) {
    std::shared_ptr<Type> tk = scope->lookup_type(target);
    if (tk != nullptr) {
      return tk;
    }
    if (scope_id == 0) {
      break;
    }
    scope_id = scope->get_parent_scope();
    scope = &m_scopes[scope_id];
  }
  return nullptr;
}

std::shared_ptr<Variable> SymTab::lookup_variable(
    const std::string& target_name) const {
  size_t scope_id = m_current_scope;
  const Scope* scope = &m_scopes[m_current_scope];
  while (true) {
    std::shared_ptr<Variable> v = scope->lookup_variable(target_name);
    if (v != nullptr) {
      return v;
    }
    if (scope_id == 0) {
      break;
    }
    scope_id = scope->get_parent_scope();
    scope = &m_scopes[scope_id];
  }
  return nullptr;
}

std::shared_ptr<Variable> SymTab::lookup_variable(
    const std::string& target_name, size_t starting_scope) {
  if (starting_scope >= m_scopes.size()) { // scope_id is unsigned
    return nullptr;
  }
  size_t old_scope = m_current_scope;
  m_current_scope = starting_scope;
  std::shared_ptr<Variable> var = lookup_variable(target_name);
  m_current_scope = old_scope;
  return var;
}

std::shared_ptr<Type> SymTab::get_primitive_type(std::string primitive) const {
  return m_scopes[0].lookup_type(Type(Type::Named(std::move(primitive)), 0));
}

std::shared_ptr<Type> SymTab::declare_type(const Type& tk) {
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

void SymTab::print(std::ostream& out) const {
  out << "Symbol Table State:\n";
  out << "Current Scope ID: " << m_current_scope << "\n";
  out << "Total Scopes: " << m_scopes.size() << "\n\n";
  for (size_t i = 0; i < m_scopes.size(); ++i) {
    out << "Scope ID: " << i << "\n";
    m_scopes[i].print(out, "  ");
    out << "\n";
  }
}
