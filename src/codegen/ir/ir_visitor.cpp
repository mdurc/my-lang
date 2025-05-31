#include "ir_visitor.h"

#include "ir_printer.h"

IrVisitor::IrVisitor() : m_last_expr_reg{-1} {}

const std::vector<IRInstruction>& IrVisitor::getInstructions() const {
  return m_ir_gen.getInstructions();
}

void IrVisitor::visit_all(const std::vector<AstPtr>& ast) {
  for (const AstPtr& ast_node : ast) {
    ast_node->accept(*this);
  }
}

void IrVisitor::unimpl(const std::string&) {
  throw std::runtime_error("Unimplemented IR feature");
}

// Expression Nodes
void IrVisitor::visit(IntegerLiteralNode& node) {
  m_last_expr_reg = m_ir_gen.newRegister();
  m_ir_gen.emitMov(m_last_expr_reg, IR_Immediate(node.value));
}

void IrVisitor::visit(IdentifierNode& node) {
  assert(m_var_registers.count(node.name) &&
         "Type checker should identifier use of undeclared identifiers");
  m_last_expr_reg = m_var_registers.at(node.name);
}

void IrVisitor::visit(BinaryOpExprNode& node) {
  node.left->accept(*this);
  IR_Register left_reg = m_last_expr_reg;

  node.right->accept(*this);
  IR_Register right_reg = m_last_expr_reg;

  m_last_expr_reg = m_ir_gen.newRegister();

  switch (node.op_type) {
    case BinOperator::Plus:
      m_ir_gen.emitAdd(m_last_expr_reg, left_reg, right_reg);
      break;
    case BinOperator::Minus:
      m_ir_gen.emitSub(m_last_expr_reg, left_reg, right_reg);
      break;
    case BinOperator::Multiply:
      m_ir_gen.emitMul(m_last_expr_reg, left_reg, right_reg);
      break;
    case BinOperator::Divide:
      m_ir_gen.emitDiv(m_last_expr_reg, left_reg, right_reg);
      break;
    default:
      unimpl("BinaryOpExprNode (non-arithmetic op)");
      // m_ir_gen.emitMov(m_last_expr_reg, IR_Immediate(0));
      break;
  }
}

void IrVisitor::visit(UnaryExprNode& node) {
  node.operand->accept(*this);
  IR_Register operand_reg = m_last_expr_reg;

  m_last_expr_reg = m_ir_gen.newRegister();

  if (node.op_type == UnaryOperator::Negate) {
    // dest = 0 - operand
    IR_Register zero_reg = m_ir_gen.newRegister();
    m_ir_gen.emitMov(zero_reg, IR_Immediate(0));
    m_ir_gen.emitSub(m_last_expr_reg, zero_reg, operand_reg);
  } else {
    unimpl("UnaryExprNode (non-negate op)");
    // m_ir_gen.emitMov(m_last_expr_reg, IR_Immediate(0));
  }
}

void IrVisitor::visit(AssignmentNode& node) {
  node.rvalue->accept(*this);
  IR_Register rval_reg = m_last_expr_reg;

  // for simplicity, expect an identifier node for lhs
  auto lval_ident = std::dynamic_pointer_cast<IdentifierNode>(node.lvalue);
  if (lval_ident) {
    assert(m_var_registers.count(lval_ident->name) &&
           "Type checker should identify use of undeclared variable");
    IR_Register lval_reg = m_var_registers.at(lval_ident->name);
    m_ir_gen.emitMov(lval_reg, rval_reg);
    m_last_expr_reg = rval_reg; // result of assignment is the rvalue
  } else {
    unimpl("AssignmentNode (complex lvalue)");
    // m_last_expr_reg = rval_reg;
  }
}

void IrVisitor::visit(GroupedExprNode& node) {
  node.expression->accept(*this);
  // Note: result is essentially discarded
}

// Statement Nodes
void IrVisitor::visit(VariableDeclNode& node) {
  assert(!m_var_registers.count(node.var_name->name) &&
         "Type checker should identify variable re-declaration");

  IR_Register var_reg = m_ir_gen.newRegister();
  m_var_registers[node.var_name->name] = var_reg;

  if (node.initializer) {
    node.initializer->accept(*this);
    m_ir_gen.emitMov(var_reg, m_last_expr_reg);
  } else {
    // TODO: default values, likely will want to put in zeros and nulls
    // for now, just leave it.
  }
}

void IrVisitor::visit(BlockNode& node) {
  // TODO: scope management for m_var_registers if blocks introduce new scopes
  for (const auto& stmt : node.statements) {
    stmt->accept(*this);
  }
}

void IrVisitor::visit(ExpressionStatementNode& node) {
  node.expression->accept(*this);
  // Note: result is essentially discarded
}

void IrVisitor::visit(FunctionDeclNode& node) {
  // TODO: handle parameters, function entry/exit, etc
  if (node.body) {
    node.body->accept(*this);
  }
}

void IrVisitor::visit(FloatLiteralNode&) { unimpl("FloatLiteralNode"); }
void IrVisitor::visit(StringLiteralNode&) { unimpl("StringLiteralNode"); }
void IrVisitor::visit(BoolLiteralNode&) { unimpl("BoolLiteralNode"); }
void IrVisitor::visit(NullLiteralNode&) { unimpl("NullLiteralNode"); }
void IrVisitor::visit(FunctionCallNode&) { unimpl("FunctionCallNode"); }
void IrVisitor::visit(MemberAccessNode&) { unimpl("MemberAccessNode"); }
void IrVisitor::visit(ArrayIndexNode&) { unimpl("ArrayIndexNode"); }
void IrVisitor::visit(StructLiteralNode&) { unimpl("StructLiteralNode"); }
void IrVisitor::visit(NewExprNode&) { unimpl("NewExprNode"); }
void IrVisitor::visit(IfStmtNode&) { unimpl("IfStmtNode"); }
void IrVisitor::visit(ForStmtNode&) { unimpl("ForStmtNode"); }
void IrVisitor::visit(WhileStmtNode&) { unimpl("WhileStmtNode"); }
void IrVisitor::visit(BreakStmtNode&) { unimpl("BreakStmtNode"); }
void IrVisitor::visit(ContinueStmtNode&) { unimpl("ContinueStmtNode"); }
void IrVisitor::visit(SwitchStmtNode&) { unimpl("SwitchStmtNode"); }
void IrVisitor::visit(ReadStmtNode&) { unimpl("ReadStmtNode"); }
void IrVisitor::visit(PrintStmtNode&) { unimpl("PrintStmtNode"); }
void IrVisitor::visit(ReturnStmtNode&) { unimpl("ReturnStmtNode"); }
void IrVisitor::visit(FreeStmtNode&) { unimpl("FreeStmtNode"); }
void IrVisitor::visit(ErrorStmtNode&) { unimpl("ErrorStmtNode"); }
void IrVisitor::visit(AsmBlockNode&) { unimpl("AsmBlockNode"); }
void IrVisitor::visit(ArgumentNode&) { unimpl("ArgumentNode"); }
void IrVisitor::visit(StructFieldInitializerNode&) {
  unimpl("StructFieldInitializerNode");
}
void IrVisitor::visit(CaseNode&) { unimpl("CaseNode"); }
void IrVisitor::visit(StructFieldNode&) { unimpl("StructFieldNode"); }
void IrVisitor::visit(ParamNode&) { unimpl("ParamNode"); }
void IrVisitor::visit(StructDeclNode&) { unimpl("StructDeclNode"); }
