#include "ir_visitor.h"

#include "../../parser/visitor.h"

IrVisitor::IrVisitor() : m_last_expr_reg(-1), m_emitted_return(false) {}

const std::vector<IRInstruction>& IrVisitor::getInstructions() const {
  return m_ir_gen.getInstructions();
}

void IrVisitor::visit_all(const std::vector<AstPtr>& ast) {
  for (const AstPtr& ast_node : ast) {
    ast_node->accept(*this);
  }
}

void IrVisitor::unimpl(const std::string& note) {
  throw std::runtime_error("Unimplemented IR feature: " + note);
}

// Expression Nodes
void IrVisitor::visit(IntegerLiteralNode& node) {
  m_last_expr_reg = m_ir_gen.newRegister();
  m_ir_gen.emitMov(m_last_expr_reg, IR_Immediate(node.value));
}

void IrVisitor::visit(BoolLiteralNode& node) {
  m_last_expr_reg = m_ir_gen.newRegister();
  m_ir_gen.emitMov(m_last_expr_reg, IR_Immediate(node.value ? 1 : 0));
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
    case BinOperator::Equal:
      m_ir_gen.emitCmpEq(m_last_expr_reg, left_reg, right_reg);
      break;
    case BinOperator::NotEqual:
      m_ir_gen.emitCmpNe(m_last_expr_reg, left_reg, right_reg);
      break;
    case BinOperator::LessThan:
      m_ir_gen.emitCmpLt(m_last_expr_reg, left_reg, right_reg);
      break;
    case BinOperator::LessEqual:
      m_ir_gen.emitCmpLe(m_last_expr_reg, left_reg, right_reg);
      break;
    case BinOperator::GreaterThan:
      m_ir_gen.emitCmpGt(m_last_expr_reg, left_reg, right_reg);
      break;
    case BinOperator::GreaterEqual:
      m_ir_gen.emitCmpGe(m_last_expr_reg, left_reg, right_reg);
      break;
    case BinOperator::LogicalAnd:
      m_ir_gen.emitLogicalAnd(m_last_expr_reg, left_reg, right_reg);
      break;
    case BinOperator::LogicalOr:
      m_ir_gen.emitLogicalOr(m_last_expr_reg, left_reg, right_reg);
      break;
    case BinOperator::Modulo:
      m_ir_gen.emitMod(m_last_expr_reg, left_reg, right_reg);
      break;
  }
}

void IrVisitor::visit(UnaryExprNode& node) {
  node.operand->accept(*this);
  IR_Register operand_reg = m_last_expr_reg;

  m_last_expr_reg = m_ir_gen.newRegister();

  switch (node.op_type) {
    case UnaryOperator::Negate:
      // dest = -operand
      m_ir_gen.emitNeg(m_last_expr_reg, operand_reg);
      break;
    case UnaryOperator::LogicalNot:
      m_ir_gen.emitLogicalNot(m_last_expr_reg, operand_reg);
      break;
    case UnaryOperator::Dereference:
    case UnaryOperator::AddressOf:
    case UnaryOperator::AddressOfMut:
      // TODO
      unimpl("UnaryExprNode (unsupported op: " +
             AstPrinter::unary_op_to_string(node.op_type) + ")");
      break;
  }
}

void IrVisitor::visit(AssignmentNode& node) {
  node.rvalue->accept(*this);
  IR_Register rval_reg = m_last_expr_reg;

  // Handle all possible L-values
  IdentPtr lval_ident = std::dynamic_pointer_cast<IdentifierNode>(node.lvalue);
  if (lval_ident) {
    assert(m_var_registers.count(lval_ident->name) &&
           "Type checker should identify use of undeclared variable");
    IR_Register lval_reg = m_var_registers.at(lval_ident->name);
    m_ir_gen.emitMov(lval_reg, rval_reg);
    m_last_expr_reg = rval_reg; // result of assignment is the rvalue
  } else {
    // TODO: UnaryExprNode, MemberAccessNode, ArrayIndexNode
    unimpl("AssignmentNode (complex lvalue)");
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
    // default initialization value (0)
    m_ir_gen.emitMov(var_reg, IR_Immediate(0));
  }
}

void IrVisitor::visit(ExpressionStatementNode& node) {
  node.expression->accept(*this);
  // Note: result is essentially discarded
}

void IrVisitor::visit(IfStmtNode& node) {
  node.condition->accept(*this);
  IR_Register cond_reg = m_last_expr_reg;

  IR_Label else_label = m_ir_gen.newLabel();
  IR_Label end_label = m_ir_gen.newLabel();

  m_ir_gen.emitGotoFalse(cond_reg, else_label);

  node.then_branch->accept(*this);

  if (node.else_branch) {
    m_ir_gen.emitGoto(end_label);
  }

  m_ir_gen.emitLabel(else_label);

  if (node.else_branch) {
    node.else_branch->accept(*this);
    m_ir_gen.emitLabel(end_label);
  }
  // else the else_branch serves as the end_label
}

void IrVisitor::visit(WhileStmtNode& node) {
  IR_Label start_loop_label = m_ir_gen.newLabel();
  IR_Label end_loop_label = m_ir_gen.newLabel();

  m_loop_contexts.push({start_loop_label, end_loop_label});

  m_ir_gen.emitLabel(start_loop_label);

  node.condition->accept(*this);
  IR_Register cond_reg = m_last_expr_reg;
  m_ir_gen.emitGotoFalse(cond_reg, end_loop_label);

  node.body->accept(*this);

  m_ir_gen.emitGoto(start_loop_label);
  m_ir_gen.emitLabel(end_loop_label);

  m_loop_contexts.pop();
}

void IrVisitor::visit(BreakStmtNode&) {
  if (m_loop_contexts.empty()) {
    throw std::runtime_error("IR Gen: Break statement outside of loop context");
  }
  m_ir_gen.emitGoto(m_loop_contexts.top().second); // break_target
}

void IrVisitor::visit(ContinueStmtNode&) {
  if (m_loop_contexts.empty()) {
    throw std::runtime_error(
        "IR Gen: Continue statement outside of loop context");
  }
  m_ir_gen.emitGoto(m_loop_contexts.top().first); // continue_target
}

void IrVisitor::visit(BlockNode& node) {
  // save current variable mappings to implement block scope
  std::unordered_map<std::string, IR_Register> prev_var_registers =
      m_var_registers;
  for (const StmtPtr& stmt : node.statements) {
    stmt->accept(*this);
  }
  // restore mappings from before the block
  m_var_registers = prev_var_registers;
}

void IrVisitor::visit(FunctionDeclNode& node) {
  std::string func_name = node.name->name;
  IR_Label func_label = m_ir_gen.newLabel();
  m_func_labels[func_name] = func_label;

  m_ir_gen.emitFunc(func_label);

  // save for scoping
  std::unordered_map<std::string, IR_Register> prev_var_registers =
      m_var_registers;
  m_var_registers.clear();

  // allocate registers for arguments
  for (const ParamPtr& param_node : node.params) {
    param_node->accept(*this);
  }

  if (node.return_type_name.has_value()) {
    IR_Register return_reg = m_ir_gen.newRegister();
    m_var_registers[node.return_type_name.value()] = return_reg;
  }

  m_emitted_return = false; // reset
  if (node.body) {
    node.body->accept(*this);
  }

  if (!m_emitted_return) {
    m_ir_gen.emitRet();
  }

  // restore var context
  m_var_registers = prev_var_registers;
}

void IrVisitor::visit(ArgumentNode& node) {
  // Much later on we will be include handling of different forms of copying, vs
  // 'give'ing and 'take'ing. For now, just accept from the expression.
  node.expression->accept(*this);
}
void IrVisitor::visit(ParamNode& node) {
  IR_Register param_reg = m_ir_gen.newRegister();
  m_var_registers[node.name->name] = param_reg;
}

void IrVisitor::visit(FunctionCallNode& node) {
  std::vector<IR_Register> arg_regs;
  for (const ArgPtr& arg_node : node.arguments) {
    arg_node->accept(*this);
    arg_regs.push_back(m_last_expr_reg);
  }

  // Emit PARAM instructions in reverse order (convention)
  for (int i = arg_regs.size() - 1; i >= 0; --i) {
    m_ir_gen.emitParam(arg_regs[i]);
  }

  IdentPtr callee_ident =
      std::dynamic_pointer_cast<IdentifierNode>(node.callee);
  if (!callee_ident) {
    unimpl("FunctionCallNode with complex callee expressions (func ptr)");
    return;
  }
  std::string func_name = callee_ident->name;
  if (!m_func_labels.count(func_name)) {
    throw std::runtime_error("IR Gen: Call to undefined function: " +
                             func_name);
  }
  IR_Label func_label = m_func_labels.at(func_name);

  std::optional<IR_Register> result_reg_opt;
  assert(node.expr_type && "Type checker should resolve function call");
  // maybe check and forbid it from being a result if it is u0 (void)
  m_last_expr_reg = m_ir_gen.newRegister();
  m_ir_gen.emitCall(result_reg_opt, func_label,
                    IR_Immediate(node.arguments.size()));
}

void IrVisitor::visit(ReturnStmtNode& node) {
  m_emitted_return = true;
  if (node.value) {
    node.value->accept(*this);
    m_ir_gen.emitRet(m_last_expr_reg);
  } else {
    // void
    m_ir_gen.emitRet();
  }
}

void IrVisitor::visit(ForStmtNode& node) {
  // (initializer; condition; iteration) body
  std::unordered_map<std::string, IR_Register> prev_var_registers;
  bool new_scope_created = false;

  if (node.initializer.has_value() &&
      std::holds_alternative<StmtPtr>(*node.initializer)) {
    // If initializer is a declaration, it creates a new scope for the loop
    prev_var_registers = m_var_registers;
    new_scope_created = true;
  }

  if (node.initializer.has_value()) {
    std::visit(
        [this](auto&& arg) {
          if (arg) arg->accept(*this);
        },
        node.initializer.value());
  }

  IR_Label condition_label = m_ir_gen.newLabel();
  IR_Label iteration_label = m_ir_gen.newLabel();
  IR_Label body_label = m_ir_gen.newLabel();
  IR_Label end_loop_label = m_ir_gen.newLabel();

  // 'continue': jump to iteration_label.
  // 'break': jump to end_loop_label.
  m_loop_contexts.push({iteration_label, end_loop_label});

  m_ir_gen.emitLabel(condition_label);
  if (node.condition) {
    node.condition->accept(*this);
    m_ir_gen.emitGotoFalse(m_last_expr_reg, end_loop_label);
  }

  m_ir_gen.emitLabel(body_label);
  node.body->accept(*this);

  m_ir_gen.emitLabel(iteration_label);
  if (node.iteration) {
    node.iteration->accept(*this);
    // the result of this iteration is discarded, it is just an expression
  }
  m_ir_gen.emitGoto(condition_label);

  m_ir_gen.emitLabel(end_loop_label);
  m_loop_contexts.pop();

  if (new_scope_created) {
    m_var_registers = prev_var_registers;
  }
}

void IrVisitor::visit(ReadStmtNode& node) {
  IdentPtr dest_ident =
      std::dynamic_pointer_cast<IdentifierNode>(node.expression);
  assert(dest_ident &&
         "Type checker should assert that we can only read from mutable "
         "integer/string variables");
  assert(m_var_registers.count(dest_ident->name) &&
         "Type checker should catch use of undeclared variables");
  IR_Register dest_reg = m_var_registers.at(dest_ident->name);
  m_ir_gen.emitRead(dest_reg);
}

void IrVisitor::visit(PrintStmtNode& node) {
  node.expression->accept(*this);
  m_ir_gen.emitPrint(m_last_expr_reg);
}

void IrVisitor::visit(FloatLiteralNode&) { unimpl("FloatLiteralNode"); }
void IrVisitor::visit(StringLiteralNode&) { unimpl("StringLiteralNode"); }
void IrVisitor::visit(NullLiteralNode&) { unimpl("NullLiteralNode"); }
void IrVisitor::visit(MemberAccessNode&) { unimpl("MemberAccessNode"); }
void IrVisitor::visit(ArrayIndexNode&) { unimpl("ArrayIndexNode"); }
void IrVisitor::visit(StructLiteralNode&) { unimpl("StructLiteralNode"); }
void IrVisitor::visit(NewExprNode&) { unimpl("NewExprNode"); }
void IrVisitor::visit(FreeStmtNode&) { unimpl("FreeStmtNode"); }
void IrVisitor::visit(ErrorStmtNode&) { unimpl("ErrorStmtNode"); }
void IrVisitor::visit(AsmBlockNode&) { unimpl("AsmBlockNode"); }
void IrVisitor::visit(StructFieldInitializerNode&) { unimpl("FieldInitNode"); }
void IrVisitor::visit(CaseNode&) { unimpl("CaseNode"); }
void IrVisitor::visit(StructFieldNode&) { unimpl("StructFieldNode"); }
void IrVisitor::visit(SwitchStmtNode&) { unimpl("SwitchStmtNode"); }
void IrVisitor::visit(StructDeclNode&) { unimpl("StructDeclNode"); }
