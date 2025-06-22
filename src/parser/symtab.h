#ifndef PARSER_SYMTAB_H
#define PARSER_SYMTAB_H

#include <memory>
#include <unordered_map>
#include <vector>

#include "ast.h"
#include "types.h"

#define DEFINE_ADD_DECL(TypeName, FuncName, MemberMap)        \
  TypeName FuncName(const std::string& name, TypeName decl) { \
    auto itr = MemberMap.insert({name, decl});                \
    return itr.second ? decl : nullptr;                       \
  }

#define DEFINE_DECLARE_DECL(TypeName, FuncName, AddFuncName)                \
  TypeName FuncName(const std::string& name, TypeName decl, size_t scope) { \
    return m_scopes[scope].AddFuncName(name, decl);                         \
  }

#define DEFINE_LOOKUP(TypeName, FuncName, MemberLookupFunc)              \
  TypeName FuncName(const std::string& name, size_t start_scope) const { \
    if (start_scope >= m_scopes.size()) {                                \
      return nullptr;                                                    \
    }                                                                    \
    for (size_t scope_id = start_scope;;                                 \
         scope_id = m_scopes[scope_id].get_parent_scope()) {             \
      TypeName result = m_scopes[scope_id].MemberLookupFunc(name);       \
      if (result) return result;                                         \
      if (scope_id == 0) break;                                          \
    }                                                                    \
    return nullptr;                                                      \
  }

struct Symbol {
  enum Kind { Variable, Type };
  Kind kind;
  std::shared_ptr<void> data;

  template <typename T>
  std::shared_ptr<T> as() const {
    return std::static_pointer_cast<T>(data);
  }
};

class Scope {
public:
  Scope(size_t parent) : m_parent_scope(parent) {}
  size_t get_parent_scope() const { return m_parent_scope; }

  template <typename T>
  std::shared_ptr<T> add(const std::string& name, Symbol::Kind kind,
                         const T& value) {
    std::shared_ptr<T> shrd_type = std::make_shared<T>(value);
    auto itr = m_symbols.insert({name, {kind, shrd_type}});
    return itr.second ? shrd_type : nullptr;
  }

  DEFINE_ADD_DECL(StructDeclPtr, add_struct, m_structs)

  const Symbol* lookup(const std::string& name) const;
  StructDeclPtr lookup_struct(const std::string& name) const;

  void print(std::ostream& out, const std::string& indent = "") const;

private:
  size_t m_parent_scope;
  std::unordered_map<std::string, Symbol> m_symbols;
  std::unordered_map<std::string, StructDeclPtr> m_structs;
};

class SymTab {
public:
  SymTab();
  void enter_scope();
  void exit_scope();

  template <typename T>
  std::shared_ptr<T> lookup(const std::string& name, size_t start_scope) const {
    if (start_scope >= m_scopes.size()) {
      return nullptr;
    }
    for (size_t scope_id = start_scope;;
         scope_id = m_scopes[scope_id].get_parent_scope()) {
      const Symbol* s = m_scopes[scope_id].lookup(name);
      if (s && s->data) return s->as<T>();
      if (scope_id == 0) break;
    }
    return nullptr;
  }

  DEFINE_LOOKUP(StructDeclPtr, lookup_struct, lookup_struct)

  template <typename T>
  std::shared_ptr<T> declare(const std::string& name, Symbol::Kind kind,
                             const T& value, size_t scope) {
    return m_scopes[scope].add<T>(name, kind, value);
  }

  DEFINE_DECLARE_DECL(StructDeclPtr, declare_struct, add_struct)

  size_t current_scope() const { return m_current_scope; }

  void print(std::ostream& out) const;

private:
  size_t m_current_scope;
  std::vector<Scope> m_scopes;
};

#endif // PARSER_SYMTAB_H
