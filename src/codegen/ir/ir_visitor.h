#ifndef CODEGEN_IR_IR_VISITOR_H
#define CODEGEN_IR_IR_VISITOR_H

#include <stack>
#include <stdexcept>
#include <string>
#include <unordered_map>

#include "../../parser/ast.h"
#include "../../parser/visitor.h"
#include "ir_generator.h"

class IrVisitor : public Visitor {
public:
  IrVisitor();

  const std::vector<IRInstruction>& getInstructions() const;
  void visit_all(const std::vector<AstPtr>& ast);

  // Expression Nodes
  void visit(IntegerLiteralNode& node) override;
  void visit(FloatLiteralNode& node) override;
  void visit(StringLiteralNode& node) override;
  void visit(BoolLiteralNode& node) override;
  void visit(NullLiteralNode& node) override;
  void visit(IdentifierNode& node) override;
  void visit(AssignmentNode& node) override;
  void visit(BinaryOpExprNode& node) override;
  void visit(UnaryExprNode& node) override;
  void visit(FunctionCallNode& node) override;
  void visit(MemberAccessNode& node) override;
  void visit(ArrayIndexNode& node) override;
  void visit(GroupedExprNode& node) override;
  void visit(StructLiteralNode& node) override;
  void visit(NewExprNode& node) override;

  // Statement Nodes
  void visit(VariableDeclNode& node) override;
  void visit(BlockNode& node) override;
  void visit(IfStmtNode& node) override;
  void visit(ForStmtNode& node) override;
  void visit(WhileStmtNode& node) override;
  void visit(BreakStmtNode& node) override;
  void visit(ContinueStmtNode& node) override;
  void visit(SwitchStmtNode& node) override;
  void visit(ReadStmtNode& node) override;
  void visit(PrintStmtNode& node) override;
  void visit(ExpressionStatementNode& node) override;
  void visit(ReturnStmtNode& node) override;
  void visit(FreeStmtNode& node) override;
  void visit(ErrorStmtNode& node) override;
  void visit(AsmBlockNode& node) override;

  // Other Nodes
  void visit(ArgumentNode& node) override;
  void visit(StructFieldInitializerNode& node) override;
  void visit(CaseNode& node) override;
  void visit(StructFieldNode& node) override;
  void visit(ParamNode& node) override;
  void visit(FunctionDeclNode& node) override;
  void visit(StructDeclNode& node) override;

private:
  IrGenerator m_ir_gen;
  std::unordered_map<std::string, IR_Register> m_var_registers;
  std::unordered_map<std::string, IR_Label> m_func_labels;
  IR_Register m_last_expr_reg;
  std::stack<std::pair<IR_Label, IR_Label>>
      m_loop_contexts; // {continue_target, break_target}
  bool m_emitted_return;

  void unimpl(const std::string& nodeName);
};

#endif // CODEGEN_IR_IR_VISITOR_H
