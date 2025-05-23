#ifndef PARSER_VISITOR_H
#define PARSER_VISITOR_H

#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

#include "ast.h"

class Visitor {
public:
  virtual ~Visitor() = default;

  // Expression Nodes
  virtual void visit(const IntegerLiteralNode& node) = 0;
  virtual void visit(const FloatLiteralNode& node) = 0;
  virtual void visit(const StringLiteralNode& node) = 0;
  virtual void visit(const BoolLiteralNode& node) = 0;
  virtual void visit(const NullLiteralNode&) = 0;
  virtual void visit(const IdentifierNode& node) = 0;
  virtual void visit(const AssignmentNode& node) = 0;
  virtual void visit(const BinaryOpExprNode& node) = 0;
  virtual void visit(const UnaryExprNode& node) = 0;
  virtual void visit(const FunctionCallNode& node) = 0;
  virtual void visit(const MemberAccessNode& node) = 0;
  virtual void visit(const ArrayIndexNode& node) = 0;
  virtual void visit(const GroupedExprNode& node) = 0;
  virtual void visit(const StructLiteralNode& node) = 0;
  virtual void visit(const NewExprNode& node) = 0;

  // Statement Nodes
  virtual void visit(const VariableDeclNode& node) = 0;
  virtual void visit(const BlockNode& node) = 0;
  virtual void visit(const IfStmtNode& node) = 0;
  virtual void visit(const ForStmtNode& node) = 0;
  virtual void visit(const WhileStmtNode& node) = 0;
  virtual void visit(const BreakStmtNode&) = 0;
  virtual void visit(const ContinueStmtNode&) = 0;
  virtual void visit(const SwitchStmtNode& node) = 0;
  virtual void visit(const PrintStmtNode& node) = 0;
  virtual void visit(const ExpressionStatementNode& node) = 0;
  virtual void visit(const ReturnStmtNode& node) = 0;
  virtual void visit(const FreeStmtNode& node) = 0;
  virtual void visit(const ErrorStmtNode& node) = 0;
  virtual void visit(const AsmBlockNode& node) = 0;

  // Other Nodes
  virtual void visit(const ArgumentNode& node) = 0;
  virtual void visit(const StructFieldInitializerNode& node) = 0;
  virtual void visit(const CaseNode& node) = 0;
  virtual void visit(const StructFieldNode& node) = 0;
  virtual void visit(const ParamNode& node) = 0;
  virtual void visit(const FunctionDeclNode& node) = 0;
  virtual void visit(const StructDeclNode& node) = 0;
};

void print_ast(const AstPtr& node, std::ostream& out);
void print_ast(const std::vector<AstPtr>& nodes, std::ostream& out);

class AstPrinter : public Visitor {
  std::ostream& out;
  int indent = 0;

  void print_indent();
  const char* bin_op_to_string(BinOperator op);
  const char* unary_op_to_string(UnaryOperator op);
  const char* borrow_state_to_string(BorrowState bs);
  void print_type(const Type& type);

public:
  AstPrinter(std::ostream& out) : out(out) {}

  void visit(const IntegerLiteralNode& node) override {
    print_indent();
    out << "Int(" << node.value << ")";
  }

  void visit(const FloatLiteralNode& node) override {
    print_indent();
    out << "Float(" << node.value << ")";
  }

  void visit(const StringLiteralNode& node) override {
    print_indent();
    out << "String(\"" << node.value << "\")";
  }

  void visit(const BoolLiteralNode& node) override {
    print_indent();
    out << (node.value ? "true" : "false");
  }

  void visit(const NullLiteralNode&) override {
    print_indent();
    out << "null";
  }

  void visit(const IdentifierNode& node) override {
    print_indent();
    out << "Ident(" << node.name << ")";
  }

  void visit(const BinaryOpExprNode& node) override {
    print_indent();
    out << "BinaryOp(" << bin_op_to_string(node.op_type) << ",\n";
    indent++;
    node.left->accept(*this);
    out << ",\n";
    node.right->accept(*this);
    indent--;
    out << "\n";
    print_indent();
    out << ")";
  }

  // Expression Nodes
  void visit(const UnaryExprNode& node) override {
    print_indent();
    out << "UnaryOp(" << unary_op_to_string(node.op_type) << ",\n";
    indent++;
    node.operand->accept(*this);
    indent--;
    out << "\n";
    print_indent();
    out << ")";
  }

  void visit(const FunctionCallNode& node) override {
    print_indent();
    out << "FunctionCall(\n";
    indent++;
    print_indent();
    out << "Callee:\n";
    indent++;
    node.callee->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "Arguments: [\n";
    indent++;
    for (size_t i = 0; i < node.arguments.size(); ++i) {
      node.arguments[i]->accept(*this);
      if (i < node.arguments.size() - 1) {
        out << ",\n";
      }
    }
    indent--;
    out << "\n";
    print_indent();
    out << "]\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const MemberAccessNode& node) override {
    print_indent();
    out << "MemberAccess(\n";
    indent++;
    print_indent();
    out << "Object:\n";
    indent++;
    node.object->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "Member:\n";
    indent++;
    node.member->accept(*this);
    indent--;
    out << "\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const ArrayIndexNode& node) override {
    print_indent();
    out << "ArrayIndex(\n";
    indent++;
    print_indent();
    out << "Object:\n";
    indent++;
    node.object->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "Index:\n";
    indent++;
    node.index->accept(*this);
    indent--;
    out << "\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const GroupedExprNode& node) override {
    print_indent();
    out << "GroupedExpr(\n";
    indent++;
    node.expression->accept(*this);
    indent--;
    out << "\n";
    print_indent();
    out << ")";
  }

  void visit(const StructLiteralNode& node) override {
    print_indent();
    out << "StructLiteral(\n";
    indent++;
    print_indent();
    out << "Type:\n";
    indent++;
    node.struct_type->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "Initializers: [\n";
    indent++;
    for (size_t i = 0; i < node.initializers.size(); ++i) {
      node.initializers[i]->accept(*this);
      if (i < node.initializers.size() - 1) {
        out << ",\n";
      }
    }
    indent--;
    out << "\n";
    print_indent();
    out << "]\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const NewExprNode& node) override {
    print_indent();
    out << "NewExpr(\n";
    indent++;
    print_indent();
    out << "TypeToAllocate:";
    if (node.is_memory_mutable) {
      out << "(mutable memory, type:";
    } else {
      out << "(immutable memory, type:";
    }
    if (node.is_array_allocation) {
      out << " array)\n";
    } else {
      out << " constructor)\n";
    }
    indent++;
    print_type(*node.type_to_allocate);
    indent--;
    out << "\n";
    if (node.allocation_specifier) {
      print_indent();
      out << "AllocationSpecifier:\n";
      indent++;
      node.allocation_specifier->accept(*this);
      indent--;
      out << "\n";
    } else {
      print_indent();
      out << "AllocationSpecifier: null\n";
    }
    indent--;
    print_indent();
    out << ")";
  }

  // Statement Nodes
  void visit(const VariableDeclNode& node) override {
    print_indent();
    out << "VariableDecl(Mutable: " << (node.is_mutable ? "true" : "false")
        << ",\n";
    indent++;
    print_indent();
    out << "Name:\n";
    indent++;
    node.var_name->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "Type:";
    if (node.type != nullptr) {
      out << "\n";
      indent++;
      print_type(*node.type);
      indent--;
      out << "\n";
    } else {
      out << " inferred\n";
    }
    if (node.initializer) {
      print_indent();
      out << "Initializer:\n";
      indent++;
      node.initializer->accept(*this);
      indent--;
      out << "\n";
    } else {
      print_indent();
      out << "Initializer: null\n";
    }
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const AssignmentNode& node) override {
    print_indent();
    out << "Assignment(\n";
    indent++;
    print_indent();
    out << "LValue:\n";
    indent++;
    node.lvalue->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "RValue:\n";
    indent++;
    node.rvalue->accept(*this);
    indent--;
    out << "\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const BlockNode& node) override {
    print_indent();
    out << "Block([\n";
    indent++;
    for (size_t i = 0; i < node.statements.size(); ++i) {
      node.statements[i]->accept(*this);
      if (i < node.statements.size() - 1) {
        out << ",";
      }
      out << "\n";
    }
    indent--;
    print_indent();
    out << "])";
  }

  void visit(const IfStmtNode& node) override {
    print_indent();
    out << "IfStmt(\n";
    indent++;
    print_indent();
    out << "Condition:\n";
    indent++;
    node.condition->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "ThenBranch:\n";
    node.then_branch->accept(*this);
    if (node.else_branch) {
      out << ",\n";
      print_indent();
      out << "ElseBranch:\n";
      node.else_branch->accept(*this);
    }
    out << "\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const ForStmtNode& node) override {
    print_indent();
    out << "ForStmt(\n";
    indent++;
    print_indent();
    out << "Initializer:";
    if (node.initializer != std::nullopt) {
      out << "\n";
      indent++;
      if (std::holds_alternative<ExprPtr>(*node.initializer)) {
        std::get<ExprPtr>(*node.initializer)->accept(*this);
      } else {
        // it must be a variable declaration StmtPtr
        assert(std::holds_alternative<StmtPtr>(*node.initializer));
        std::get<StmtPtr>(*node.initializer)->accept(*this);
      }
      indent--;
    } else {
      out << " null";
    }
    out << ",\n";
    print_indent();
    out << "Condition:";
    if (node.condition) {
      out << "\n";
      indent++;
      node.condition->accept(*this);
      indent--;
    } else {
      out << "null";
    }
    out << ",\n";
    print_indent();
    out << "Iteration:";
    if (node.iteration) {
      out << "\n";
      indent++;
      node.iteration->accept(*this);
      indent--;
    } else {
      out << "null";
    }
    out << ",\n";
    print_indent();
    out << "Body:\n";
    node.body->accept(*this);
    out << "\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const WhileStmtNode& node) override {
    print_indent();
    out << "WhileStmt(\n";
    indent++;
    print_indent();
    out << "Condition:\n";
    indent++;
    node.condition->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "Body:\n";
    node.body->accept(*this);
    out << "\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const BreakStmtNode&) override {
    print_indent();
    out << "BreakStmt";
  }

  void visit(const ContinueStmtNode&) override {
    print_indent();
    out << "ContinueStmt";
  }

  void visit(const SwitchStmtNode& node) override {
    print_indent();
    out << "SwitchStmt(\n";
    indent++;
    print_indent();
    out << "Expression:\n";
    indent++;
    node.expression->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "Cases: [\n";
    indent++;
    for (size_t i = 0; i < node.cases.size(); ++i) {
      node.cases[i]->accept(*this);
      if (i < node.cases.size() - 1) {
        out << ",";
      }
      out << "\n";
    }
    indent--;
    print_indent();
    out << "]\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const PrintStmtNode& node) override {
    print_indent();
    out << "PrintStmt(\n";
    indent++;
    node.expression->accept(*this);
    indent--;
    out << "\n";
    print_indent();
    out << ")";
  }

  void visit(const ExpressionStatementNode& node) override {
    print_indent();
    out << "ExpressionStmt(\n";
    indent++;
    node.expression->accept(*this);
    indent--;
    out << "\n";
    print_indent();
    out << ")";
  }

  void visit(const ReturnStmtNode& node) override {
    print_indent();
    out << "ReturnStmt(";
    if (node.value) {
      out << "\n";
      indent++;
      node.value->accept(*this);
      indent--;
      out << "\n";
      print_indent();
    }
    out << ")";
  }

  void visit(const FreeStmtNode& node) override {
    print_indent();
    out << "FreeStmt(IsArray: "
        << (node.is_array_deallocation ? "true" : "false") << ",\n";
    indent++;
    node.expression->accept(*this);
    indent--;
    out << "\n";
    print_indent();
    out << ")";
  }

  void visit(const ErrorStmtNode& node) override {
    print_indent();
    out << "ErrorStmt(Message: \"" << node.message_content << "\")";
  }

  void visit(const AsmBlockNode& node) override {
    print_indent();
    out << "AsmBlock(\n";
    indent++;
    print_indent();
    out << "Body: \"\"\"\n";
    std::istringstream iss(node.body);
    std::string line;
    while (std::getline(iss, line)) {
      print_indent();
      out << line << "\n";
    }
    print_indent();
    out << "\"\"\"\n";
    indent--;
    print_indent();
    out << ")";
  }

  // Other Nodes
  void visit(const ArgumentNode& node) override {
    print_indent();
    out << "Argument(IsGive: " << (node.is_give ? "true" : "false") << ",\n";
    indent++;
    node.expression->accept(*this);
    indent--;
    out << "\n";
    print_indent();
    out << ")";
  }

  void visit(const StructFieldInitializerNode& node) override {
    print_indent();
    out << "StructFieldInitializer(\n";
    indent++;
    print_indent();
    out << "Field:\n";
    indent++;
    node.field->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "Value:\n";
    indent++;
    node.value->accept(*this);
    indent--;
    out << "\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const CaseNode& node) override {
    print_indent();
    out << "Case(\n";
    indent++;
    print_indent();
    out << "Value:";
    if (node.value) {
      out << "\n";
      indent++;
      node.value->accept(*this);
      indent--;
    } else {
      out << "default";
    }
    out << ",\n";
    print_indent();
    out << "Body:\n";
    node.body->accept(*this);
    out << "\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const StructFieldNode& node) override {
    print_indent();
    out << "StructField(\n";
    indent++;
    print_indent();
    out << "Name:\n";
    indent++;
    node.name->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "Type:\n";
    indent++;
    print_type(*node.type);
    indent--;
    out << "\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const ParamNode& node) override {
    print_indent();
    out << "Param(Modifier: " << borrow_state_to_string(node.modifier) << ",\n";
    indent++;
    print_indent();
    out << "Name:\n";
    indent++;
    node.name->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "Type:\n";
    indent++;
    print_type(*node.type);
    indent--;
    out << "\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const FunctionDeclNode& node) override {
    print_indent();
    out << "FunctionDecl(\n";
    indent++;
    print_indent();
    out << "Name:\n";
    indent++;
    node.name->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "Params: [\n";
    indent++;
    for (size_t i = 0; i < node.params.size(); ++i) {
      node.params[i]->accept(*this);
      if (i < node.params.size() - 1) {
        out << ",";
      }
      out << "\n";
    }
    indent--;
    print_indent();
    out << "],\n";
    print_indent();
    out << "ReturnType:";
    if (node.return_type_name != std::nullopt) {
      out << " (NamedVar:" << *node.return_type_name << ")";
    }
    out << '\n';
    indent++;
    print_type(*node.return_type);
    indent--;
    out << ",\n";
    print_indent();
    out << "Body:\n";
    node.body->accept(*this);
    out << "\n";
    indent--;
    print_indent();
    out << ")";
  }

  void visit(const StructDeclNode& node) override {
    print_indent();
    out << "StructDecl(\n";
    indent++;
    print_indent();
    out << "Name:\n";
    indent++;
    node.name->accept(*this);
    indent--;
    out << ",\n";
    print_indent();
    out << "Members: [\n";
    indent++;
    for (size_t i = 0; i < node.members.size(); ++i) {
      std::visit([this](auto&& arg) { arg->accept(*this); }, node.members[i]);
      if (i < node.members.size() - 1) {
        out << ",";
      }
      out << "\n";
    }
    indent--;
    print_indent();
    out << "]\n";
    indent--;
    print_indent();
    out << ")";
  }
};

#endif
