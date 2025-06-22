#include "json_exporter.h"

#include <nlohmann/json.hpp>
#include <string>
#include <vector>

using json = nlohmann::json;

static json span_to_range(const Span& span) {
  return {{"start", {{"line", span.row}, {"character", span.start_col}}},
          {"end", {{"line", span.row}, {"character", span.end_col}}}};
}

static void collect_hover(const AstPtr& node, json& hover) {
  if (!node || !node->token) return;

  // Ident
  if (auto ident = dynamic_cast<const IdentifierNode*>(node.get())) {
    json h;
    h["range"] = span_to_range(ident->token->get_span());
    h["contents"] = ident->name;
    if (ident->expr_type) {
      h["type"] = ident->expr_type->to_string();
    }
    hover.push_back(h);
  }

  // Literals
  if (auto int_lit = dynamic_cast<const IntegerLiteralNode*>(node.get())) {
    json h;
    h["range"] = span_to_range(int_lit->token->get_span());
    h["contents"] = std::to_string(int_lit->value);
    if (int_lit->expr_type) {
      h["type"] = int_lit->expr_type->to_string();
    }
    hover.push_back(h);
  } else if (auto float_lit =
                 dynamic_cast<const FloatLiteralNode*>(node.get())) {
    json h;
    h["range"] = span_to_range(float_lit->token->get_span());
    h["contents"] = std::to_string(float_lit->value);
    if (float_lit->expr_type) {
      h["type"] = float_lit->expr_type->to_string();
    }
    hover.push_back(h);
  } else if (auto str_lit =
                 dynamic_cast<const StringLiteralNode*>(node.get())) {
    json h;
    h["range"] = span_to_range(str_lit->token->get_span());
    h["contents"] = str_lit->value;
    if (str_lit->expr_type) {
      h["type"] = str_lit->expr_type->to_string();
    }
    hover.push_back(h);
  } else if (auto bool_lit = dynamic_cast<const BoolLiteralNode*>(node.get())) {
    json h;
    h["range"] = span_to_range(bool_lit->token->get_span());
    h["contents"] = bool_lit->value ? "true" : "false";
    if (bool_lit->expr_type) {
      h["type"] = bool_lit->expr_type->to_string();
    }
    hover.push_back(h);
  } else if (auto null_lit = dynamic_cast<const NullLiteralNode*>(node.get())) {
    json h;
    h["range"] = span_to_range(null_lit->token->get_span());
    h["contents"] = "null";
    if (null_lit->expr_type) {
      h["type"] = null_lit->expr_type->to_string();
    }
    hover.push_back(h);
  }

  // ExpressionNode: check for sub-expressions
  if (auto assign = dynamic_cast<const AssignmentNode*>(node.get())) {
    collect_hover(assign->lvalue, hover);
    collect_hover(assign->rvalue, hover);
  } else if (auto bin = dynamic_cast<const BinaryOpExprNode*>(node.get())) {
    collect_hover(bin->left, hover);
    collect_hover(bin->right, hover);
  } else if (auto un = dynamic_cast<const UnaryExprNode*>(node.get())) {
    collect_hover(un->operand, hover);
  } else if (auto call = dynamic_cast<const FunctionCallNode*>(node.get())) {
    collect_hover(call->callee, hover);
    for (const ArgPtr& arg : call->arguments) {
      if (arg && arg->expression) {
        collect_hover(arg->expression, hover);
      }
    }
  } else if (auto field = dynamic_cast<const FieldAccessNode*>(node.get())) {
    collect_hover(field->object, hover);
    collect_hover(field->field, hover);
  } else if (auto arr = dynamic_cast<const ArrayIndexNode*>(node.get())) {
    collect_hover(arr->object, hover);
    collect_hover(arr->index, hover);
  } else if (auto group = dynamic_cast<const GroupedExprNode*>(node.get())) {
    collect_hover(group->expression, hover);
  } else if (auto struct_lit =
                 dynamic_cast<const StructLiteralNode*>(node.get())) {
    for (const StructFieldInitPtr& init : struct_lit->initializers) {
      if (init && init->field) {
        collect_hover(init->field, hover);
      }
      if (init && init->value) {
        collect_hover(init->value, hover);
      }
    }
  } else if (auto new_expr = dynamic_cast<const NewExprNode*>(node.get())) {
    if (new_expr->allocation_specifier) {
      collect_hover(new_expr->allocation_specifier, hover);
    }
  }

  // StatementNode: check for sub-statements/expressions
  if (auto var = dynamic_cast<const VariableDeclNode*>(node.get())) {
    if (var->var_name) {
      collect_hover(var->var_name, hover);
    }
    if (var->initializer) {
      collect_hover(var->initializer, hover);
    }
  } else if (auto block = dynamic_cast<const BlockNode*>(node.get())) {
    for (const StmtPtr& stmt : block->statements) {
      collect_hover(stmt, hover);
    }
  } else if (auto ifs = dynamic_cast<const IfStmtNode*>(node.get())) {
    collect_hover(ifs->condition, hover);
    if (ifs->then_branch) {
      collect_hover(ifs->then_branch, hover);
    }
    if (ifs->else_branch) {
      collect_hover(ifs->else_branch, hover);
    }
  } else if (auto fors = dynamic_cast<const ForStmtNode*>(node.get())) {
    if (fors->initializer) {
      if (std::holds_alternative<ExprPtr>(*fors->initializer)) {
        collect_hover(std::get<ExprPtr>(*fors->initializer), hover);
      } else if (std::holds_alternative<StmtPtr>(*fors->initializer)) {
        collect_hover(std::get<StmtPtr>(*fors->initializer), hover);
      }
    }
    if (fors->condition) {
      collect_hover(fors->condition, hover);
    }
    if (fors->iteration) {
      collect_hover(fors->iteration, hover);
    }
    if (fors->body) {
      collect_hover(fors->body, hover);
    }
  } else if (auto whiles = dynamic_cast<const WhileStmtNode*>(node.get())) {
    if (whiles->condition) {
      collect_hover(whiles->condition, hover);
    }
    if (whiles->body) {
      collect_hover(whiles->body, hover);
    }
  } else if (auto sw = dynamic_cast<const SwitchStmtNode*>(node.get())) {
    if (sw->expression) {
      collect_hover(sw->expression, hover);
    }
    for (const CasePtr& c : sw->cases) {
      if (c) {
        collect_hover(c, hover);
      }
    }
  } else if (auto cs = dynamic_cast<const CaseNode*>(node.get())) {
    if (cs->value) {
      collect_hover(cs->value, hover);
    }
    if (cs->body) {
      collect_hover(cs->body, hover);
    }
  } else if (auto print = dynamic_cast<const PrintStmtNode*>(node.get())) {
    for (const ExprPtr& e : print->expressions) {
      collect_hover(e, hover);
    }
  } else if (auto exprstmt =
                 dynamic_cast<const ExpressionStatementNode*>(node.get())) {
    if (exprstmt->expression) {
      collect_hover(exprstmt->expression, hover);
    }
  } else if (auto ret = dynamic_cast<const ReturnStmtNode*>(node.get())) {
    if (ret->value) {
      collect_hover(ret->value, hover);
    }
  } else if (auto free_stmt = dynamic_cast<const FreeStmtNode*>(node.get())) {
    if (free_stmt->expression) {
      collect_hover(free_stmt->expression, hover);
    }
  } else if (auto err = dynamic_cast<const ErrorStmtNode*>(node.get())) {
    // no sub-nodes
  } else if (auto exit = dynamic_cast<const ExitStmtNode*>(node.get())) {
    // no sub-nodes
  } else if (auto asmblk = dynamic_cast<const AsmBlockNode*>(node.get())) {
    // no sub-nodes
  } else if (auto struct_field =
                 dynamic_cast<const StructFieldNode*>(node.get())) {
    if (struct_field->name) {
      collect_hover(struct_field->name, hover);
    }
  } else if (auto struct_decl =
                 dynamic_cast<const StructDeclNode*>(node.get())) {
    for (const StructFieldPtr& f : struct_decl->fields) {
      if (f) {
        collect_hover(f, hover);
      }
    }
  } else if (auto param = dynamic_cast<const ParamNode*>(node.get())) {
    if (param->name) {
      collect_hover(param->name, hover);
    }
  } else if (auto func = dynamic_cast<const FunctionDeclNode*>(node.get())) {
    if (func->name) {
      collect_hover(func->name, hover);
    }
    for (const auto& p : func->params) {
      if (p) {
        collect_hover(p, hover);
      }
    }
    if (func->body) {
      collect_hover(func->body, hover);
    }
  }
}

static void collect_definitions(const SymTab* symtab, json& definitions) {
  if (!symtab) {
    return;
  }

  const std::vector<Scope>& scopes = symtab->get_scopes();
  for (size_t scope_id = 0; scope_id < scopes.size(); ++scope_id) {
    const Scope& scope = scopes[scope_id];
    // variables and types
    for (const auto& [name, symbol] : scope.get_symbols()) {
      bool is_var = symbol.kind == Symbol::Variable;

      json def;
      def["name"] = name;
      def["kind"] = is_var ? "variable" : "type";
      def["scope_id"] = scope_id;

      if (is_var) {
        std::shared_ptr<Variable> var = symbol.as<Variable>();
        if (var && var->type) {
          def["type"] = var->type->to_string();
        }
        def["modifier"] = variable_borrowed_state_to_string(var->modifier);
        def["def_scope_id"] = var->scope_id;
      } else if (symbol.kind == Symbol::Type) {
        std::shared_ptr<Type> type = symbol.as<Type>();
        if (type) {
          def["type"] = type->to_string();
        }
      }
      definitions.push_back(def);
    }
  }
}

std::string JsonExporter::export_to_json() {
  json result;

  // == Diagnostics ==
  json diagnostics = json::array();
  for (const Diagnostic& d : m_logger->get_fatals()) {
    diagnostics.push_back({{"range", span_to_range(d.get_span())},
                           {"message", d.what()},
                           {"severity", 1}});
  }
  for (const Diagnostic& d : m_logger->get_errors()) {
    diagnostics.push_back({{"range", span_to_range(d.get_span())},
                           {"message", d.what()},
                           {"severity", 1}});
  }
  for (const Diagnostic& d : m_logger->get_warnings()) {
    diagnostics.push_back({{"range", span_to_range(d.get_span())},
                           {"message", d.what()},
                           {"severity", 2}});
  }
  result["diagnostics"] = diagnostics;

  // == Goto Definitions ==
  json definitions = json::array();
  collect_definitions(m_symtab, definitions);
  result["definitions"] = definitions;

  // == Hover ==
  json hover = json::array();
  if (m_ast_nodes) {
    for (const AstPtr& node : *m_ast_nodes) {
      collect_hover(node, hover);
    }
  }
  result["hover"] = hover;

  return result.dump(2);
}
