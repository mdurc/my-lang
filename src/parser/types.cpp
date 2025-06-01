#include "types.h"

std::string Type::to_string_recursive(std::vector<const Type*>& visited) const {
  for (const Type* t : visited) {
    if (t == this) {
      if (std::holds_alternative<Named>(m_storage)) {
        return "[recursive_ref_to:" + std::get<Named>(m_storage).identifier +
               "]";
      }
      return "[recursive_type_ref]";
    }
  }
  visited.push_back(this);

  std::string str;

  if (this->is<Named>()) {
    str = this->as<Named>().identifier;
  } else if (this->is<Function>()) {
    const Function& func = this->as<Function>();
    str = "func(";
    for (size_t i = 0; i < func.params.size(); ++i) {
      if (func.params[i]) {
        str += func.params[i]->to_string_recursive(visited);
      } else {
        str += "uninitialized_param_type";
      }
      if (i < func.params.size() - 1) str += ", ";
    }
    str += ")->";
    if (func.return_type) {
      str += func.return_type->to_string_recursive(visited);
    } else {
      // this shouldn't happen even if there is no returns clause because
      // it should return a u0 type
      str += "uninitialized_return_type";
    }
  } else if (this->is<Pointer>()) {
    const Pointer& ptr = std::get<Pointer>(m_storage);
    str = "ptr<";
    str += (ptr.is_pointee_mutable ? "mut " : "imm ");
    if (ptr.pointee) {
      str += ptr.pointee->to_string_recursive(visited);
    } else {
      str += "uninitialized_pointee_type";
    }
    str += ">";
  }
  visited.pop_back();
  return str;
}

std::string Type::to_string() const {
  std::vector<const Type*> visited;
  return to_string_recursive(visited);
}
