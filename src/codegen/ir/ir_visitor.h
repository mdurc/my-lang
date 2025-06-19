#ifndef CODEGEN_IR_IR_VISITOR_H
#define CODEGEN_IR_IR_VISITOR_H

#include <set>
#include <stack>
#include <stdexcept>
#include <string>
#include <unordered_map>

#include "../../parser/ast.h"
#include "../../parser/symtab.h"
#include "../../parser/visitor.h"
#include "ir_generator.h"

class IrVisitor : public Visitor {
public:
  IrVisitor(const SymTab* tab);

  const std::vector<IRInstruction>& get_instructions() const;
  bool is_main_defined() const { return m_main_function_defined; }
  void visit_all(const std::vector<AstPtr>& ast);

  // variable helpers to make sure that we take scopes into account + shadowing
  bool var_exists(const std::string& name, size_t scope_id);
  const IR_Variable& get_var(const std::string& name, size_t scope_id);
  const IR_Variable* add_var(const std::string& name, size_t scope_id,
                             bool is_func_decl);

  // Expression Nodes
  void visit(IntegerLiteralNode& node) override;
  void visit(FloatLiteralNode& node) override;
  void visit(StringLiteralNode& node) override;
  void visit(BoolLiteralNode& node) override;
  void visit(NullLiteralNode& node) override;
  void visit(IdentifierNode& node) override;
  void visit(AssignmentNode& node) override;
  void visit(BinaryOpExprNode& node) override;

  void visit_addrof(const ExprPtr& op, const IR_Register& dst);
  void visit(UnaryExprNode& node) override;

  void call_func_name(const std::string& func_name, size_t scope_id,
                      std::shared_ptr<Type> expr_type);
  void visit(FunctionCallNode& node) override;
  void visit(FieldAccessNode& node) override;
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
  void visit(ExitStmtNode& node) override;
  void visit(AsmBlockNode& node) override;

  // Other Nodes
  void visit(ArgumentNode& node) override;
  void visit(StructFieldInitializerNode& node) override;
  void visit(CaseNode&) override { /* handled within switchstmtnode */ };
  void visit(StructFieldNode& node) override;
  void visit(ParamNode& node) override;
  void visit(FunctionDeclNode& node) override;
  void visit(StructDeclNode& node) override;

private:
  const SymTab* m_symtab;
  IrGenerator m_ir_gen;
  std::set<IR_Variable> m_vars;
  std::stack<std::pair<IR_Label, IR_Label>> m_loop_contexts; // {continue,break}

  IROperand m_last_expr_operand;
  bool m_emitted_return;
  bool m_main_function_defined;

  IR_Label get_runtime_print_call(const std::shared_ptr<Type>& type);

  IR_Register compute_struct_field_addr(const ExprPtr& object,
                                        const std::string& field_name);

  void unimpl(const std::string& nodeName);
};

#endif // CODEGEN_IR_IR_VISITOR_H
