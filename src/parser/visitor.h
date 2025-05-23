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

  void printIndent();
  const char* binOpToString(BinOperator op);
  const char* unaryOpToString(UnaryOperator op);
  const char* borrowStateToString(BorrowState bs);
  void printType(const Type& type);

public:
  AstPrinter(std::ostream& out) : out(out) {}

  void visit(const IntegerLiteralNode& node) override {
    printIndent();
    out << "Int(" << node.value << ")";
  }

  void visit(const FloatLiteralNode& node) override {
    printIndent();
    out << "Float(" << node.value << ")";
  }

  void visit(const StringLiteralNode& node) override {
    printIndent();
    out << "String(\"" << node.value << "\")";
  }

  void visit(const BoolLiteralNode& node) override {
    printIndent();
    out << (node.value ? "true" : "false");
  }

  void visit(const NullLiteralNode&) override {
    printIndent();
    out << "null";
  }

  void visit(const IdentifierNode& node) override {
    printIndent();
    out << "Ident(" << node.name << ")";
  }

  void visit(const BinaryOpExprNode& node) override {
    printIndent();
    out << "BinaryOp(" << binOpToString(node.op_type) << ",\n";
    indent++;
    node.left->accept(*this);
    out << ",\n";
    node.right->accept(*this);
    indent--;
    out << "\n";
    printIndent();
    out << ")";
  }

  // Expression Nodes
  void visit(const UnaryExprNode& node) override {
    printIndent();
    out << "UnaryOp(" << unaryOpToString(node.op_type) << ",\n";
    indent++;
    node.operand->accept(*this);
    indent--;
    out << "\n";
    printIndent();
    out << ")";
  }

  void visit(const FunctionCallNode& node) override {
    printIndent();
    out << "FunctionCall(\n";
    indent++;
    printIndent();
    out << "Callee:\n";
    indent++;
    node.callee->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
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
    printIndent();
    out << "]\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const MemberAccessNode& node) override {
    printIndent();
    out << "MemberAccess(\n";
    indent++;
    printIndent();
    out << "Object:\n";
    indent++;
    node.object->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
    out << "Member:\n";
    indent++;
    node.member->accept(*this);
    indent--;
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const ArrayIndexNode& node) override {
    printIndent();
    out << "ArrayIndex(\n";
    indent++;
    printIndent();
    out << "Object:\n";
    indent++;
    node.object->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
    out << "Index:\n";
    indent++;
    node.index->accept(*this);
    indent--;
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const GroupedExprNode& node) override {
    printIndent();
    out << "GroupedExpr(\n";
    indent++;
    node.expression->accept(*this);
    indent--;
    out << "\n";
    printIndent();
    out << ")";
  }

  void visit(const StructLiteralNode& node) override {
    printIndent();
    out << "StructLiteral(\n";
    indent++;
    printIndent();
    out << "Type:\n";
    indent++;
    node.struct_type->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
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
    printIndent();
    out << "]\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const NewExprNode& node) override {
    printIndent();
    out << "NewExpr(\n";
    indent++;
    printIndent();
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
    printType(*node.type_to_allocate);
    indent--;
    out << "\n";
    if (node.allocation_specifier) {
      printIndent();
      out << "AllocationSpecifier:\n";
      indent++;
      node.allocation_specifier->accept(*this);
      indent--;
      out << "\n";
    } else {
      printIndent();
      out << "AllocationSpecifier: null\n";
    }
    indent--;
    printIndent();
    out << ")";
  }

  // Statement Nodes
  void visit(const VariableDeclNode& node) override {
    printIndent();
    out << "VariableDecl(Mutable: " << (node.is_mutable ? "true" : "false")
        << ",\n";
    indent++;
    printIndent();
    out << "Name:\n";
    indent++;
    node.var_name->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
    out << "Type:";
    if (node.type != nullptr) {
      out << "\n";
      indent++;
      printType(*node.type);
      indent--;
      out << "\n";
    } else {
      out << "inferred\n";
    }
    if (node.initializer) {
      printIndent();
      out << "Initializer:\n";
      indent++;
      node.initializer->accept(*this);
      indent--;
      out << "\n";
    } else {
      printIndent();
      out << "Initializer: null\n";
    }
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const AssignmentNode& node) override {
    printIndent();
    out << "Assignment(\n";
    indent++;
    printIndent();
    out << "LValue:\n";
    indent++;
    node.lvalue->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
    out << "RValue:\n";
    indent++;
    node.rvalue->accept(*this);
    indent--;
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const BlockNode& node) override {
    printIndent();
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
    printIndent();
    out << "])";
  }

  void visit(const IfStmtNode& node) override {
    printIndent();
    out << "IfStmt(\n";
    indent++;
    printIndent();
    out << "Condition:\n";
    indent++;
    node.condition->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
    out << "ThenBranch:\n";
    node.then_branch->accept(*this);
    if (node.else_branch) {
      out << ",\n";
      printIndent();
      out << "ElseBranch:\n";
      node.else_branch->accept(*this);
    }
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const ForStmtNode& node) override {
    printIndent();
    out << "ForStmt(\n";
    indent++;
    printIndent();
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
    printIndent();
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
    printIndent();
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
    printIndent();
    out << "Body:\n";
    node.body->accept(*this);
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const WhileStmtNode& node) override {
    printIndent();
    out << "WhileStmt(\n";
    indent++;
    printIndent();
    out << "Condition:\n";
    indent++;
    node.condition->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
    out << "Body:\n";
    node.body->accept(*this);
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const BreakStmtNode&) override {
    printIndent();
    out << "BreakStmt";
  }

  void visit(const ContinueStmtNode&) override {
    printIndent();
    out << "ContinueStmt";
  }

  void visit(const SwitchStmtNode& node) override {
    printIndent();
    out << "SwitchStmt(\n";
    indent++;
    printIndent();
    out << "Expression:\n";
    indent++;
    node.expression->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
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
    printIndent();
    out << "]\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const PrintStmtNode& node) override {
    printIndent();
    out << "PrintStmt(\n";
    indent++;
    node.expression->accept(*this);
    indent--;
    out << "\n";
    printIndent();
    out << ")";
  }

  void visit(const ExpressionStatementNode& node) override {
    printIndent();
    out << "ExpressionStmt(\n";
    indent++;
    node.expression->accept(*this);
    indent--;
    out << "\n";
    printIndent();
    out << ")";
  }

  void visit(const ReturnStmtNode& node) override {
    printIndent();
    out << "ReturnStmt(";
    if (node.value) {
      out << "\n";
      indent++;
      node.value->accept(*this);
      indent--;
      out << "\n";
      printIndent();
    }
    out << ")";
  }

  void visit(const FreeStmtNode& node) override {
    printIndent();
    out << "FreeStmt(IsArray: "
        << (node.is_array_deallocation ? "true" : "false") << ",\n";
    indent++;
    node.expression->accept(*this);
    indent--;
    out << "\n";
    printIndent();
    out << ")";
  }

  void visit(const ErrorStmtNode& node) override {
    printIndent();
    out << "ErrorStmt(Message: \"" << node.message_content << "\")";
  }

  void visit(const AsmBlockNode& node) override {
    printIndent();
    out << "AsmBlock(\n";
    indent++;
    printIndent();
    out << "Body: \"\"\"\n";
    std::istringstream iss(node.body);
    std::string line;
    while (std::getline(iss, line)) {
      printIndent();
      out << line << "\n";
    }
    printIndent();
    out << "\"\"\"\n";
    indent--;
    printIndent();
    out << ")";
  }

  // Other Nodes
  void visit(const ArgumentNode& node) override {
    printIndent();
    out << "Argument(IsGive: " << (node.is_give ? "true" : "false") << ",\n";
    indent++;
    node.expression->accept(*this);
    indent--;
    out << "\n";
    printIndent();
    out << ")";
  }

  void visit(const StructFieldInitializerNode& node) override {
    printIndent();
    out << "StructFieldInitializer(\n";
    indent++;
    printIndent();
    out << "Field:\n";
    indent++;
    node.field->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
    out << "Value:\n";
    indent++;
    node.value->accept(*this);
    indent--;
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const CaseNode& node) override {
    printIndent();
    out << "Case(\n";
    indent++;
    printIndent();
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
    printIndent();
    out << "Body:\n";
    node.body->accept(*this);
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const StructFieldNode& node) override {
    printIndent();
    out << "StructField(\n";
    indent++;
    printIndent();
    out << "Name:\n";
    indent++;
    node.name->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
    out << "Type:\n";
    indent++;
    printType(*node.type);
    indent--;
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const ParamNode& node) override {
    printIndent();
    out << "Param(Modifier: " << borrowStateToString(node.modifier) << ",\n";
    indent++;
    printIndent();
    out << "Name:\n";
    indent++;
    node.name->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
    out << "Type:\n";
    indent++;
    printType(*node.type);
    indent--;
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const FunctionDeclNode& node) override {
    printIndent();
    out << "FunctionDecl(\n";
    indent++;
    printIndent();
    out << "Name:\n";
    indent++;
    node.name->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
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
    printIndent();
    out << "],\n";
    printIndent();
    out << "ReturnType:";
    if (node.return_type_name != std::nullopt) {
      out << " (NamedVar:" << *node.return_type_name << ")";
    }
    out << '\n';
    indent++;
    printType(*node.return_type);
    indent--;
    out << ",\n";
    printIndent();
    out << "Body:\n";
    node.body->accept(*this);
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  }

  void visit(const StructDeclNode& node) override {
    printIndent();
    out << "StructDecl(\n";
    indent++;
    printIndent();
    out << "Name:\n";
    indent++;
    node.name->accept(*this);
    indent--;
    out << ",\n";
    printIndent();
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
    printIndent();
    out << "]\n";
    indent--;
    printIndent();
    out << ")";
  }
};

#endif
