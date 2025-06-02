#include "ir_visitor.h"

#include "../../parser/visitor.h"

IrVisitor::IrVisitor()
    : m_last_expr_operand(IR_Register(-1)),
      m_emitted_return(false),
      m_main_function_defined(false) {}

const std::vector<IRInstruction>& IrVisitor::get_instructions() const {
  return m_ir_gen.get_instructions();
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
  IR_Register temp_reg = m_ir_gen.new_temp_reg();
  m_ir_gen.emit_assign(temp_reg, IR_Immediate(node.value));
  m_last_expr_operand = temp_reg;
}

void IrVisitor::visit(BoolLiteralNode& node) {
  IR_Register temp_reg = m_ir_gen.new_temp_reg();
  m_ir_gen.emit_assign(temp_reg, IR_Immediate(node.value ? 1 : 0));
  m_last_expr_operand = temp_reg;
}

void IrVisitor::visit(StringLiteralNode& node) {
  // backend handles putting string in memory
  IR_Register temp_reg = m_ir_gen.new_temp_reg();
  m_ir_gen.emit_assign(temp_reg, node.value);
  m_last_expr_operand = temp_reg;
}

void IrVisitor::visit(IdentifierNode& node) {
  assert(m_vars.count(node.name) &&
         "Type checker should identifier use of undeclared identifiers");
  m_last_expr_operand = m_vars.at(node.name);
}

void IrVisitor::visit(BinaryOpExprNode& node) {
  node.left->accept(*this);
  IROperand left_op = m_last_expr_operand;

  node.right->accept(*this);
  IROperand right_op = m_last_expr_operand;

  IR_Register dest_reg = m_ir_gen.new_temp_reg();

  switch (node.op_type) {
    case BinOperator::Plus:
      m_ir_gen.emit_add(dest_reg, left_op, right_op);
      break;
    case BinOperator::Minus:
      m_ir_gen.emit_sub(dest_reg, left_op, right_op);
      break;
    case BinOperator::Multiply:
      m_ir_gen.emit_mul(dest_reg, left_op, right_op);
      break;
    case BinOperator::Divide:
      m_ir_gen.emit_div(dest_reg, left_op, right_op);
      break;
    case BinOperator::Equal:
      m_ir_gen.emit_cmp_eq(dest_reg, left_op, right_op);
      break;
    case BinOperator::NotEqual:
      m_ir_gen.emit_cmp_ne(dest_reg, left_op, right_op);
      break;
    case BinOperator::LessThan:
      m_ir_gen.emit_cmp_lt(dest_reg, left_op, right_op);
      break;
    case BinOperator::LessEqual:
      m_ir_gen.emit_cmp_le(dest_reg, left_op, right_op);
      break;
    case BinOperator::GreaterThan:
      m_ir_gen.emit_cmp_gt(dest_reg, left_op, right_op);
      break;
    case BinOperator::GreaterEqual:
      m_ir_gen.emit_cmp_ge(dest_reg, left_op, right_op);
      break;
    case BinOperator::LogicalAnd:
      m_ir_gen.emit_logical_and(dest_reg, left_op, right_op);
      break;
    case BinOperator::LogicalOr:
      m_ir_gen.emit_logical_or(dest_reg, left_op, right_op);
      break;
    case BinOperator::Modulo:
      m_ir_gen.emit_mod(dest_reg, left_op, right_op);
      break;
  }

  m_last_expr_operand = dest_reg;
}

void IrVisitor::visit(UnaryExprNode& node) {
  node.operand->accept(*this);
  IROperand operand_op = m_last_expr_operand;

  IR_Register dest_reg = m_ir_gen.new_temp_reg();

  switch (node.op_type) {
    case UnaryOperator::Negate:
      // dest = -operand
      m_ir_gen.emit_neg(dest_reg, operand_op);
      break;
    case UnaryOperator::LogicalNot:
      m_ir_gen.emit_logical_not(dest_reg, operand_op);
      break;
    case UnaryOperator::Dereference:
      m_ir_gen.emit_load(dest_reg, operand_op);
      break;
    case UnaryOperator::AddressOf:
    case UnaryOperator::AddressOfMut:
      // TODO addressof and addressofmut
      unimpl("UnaryExprNode (unsupported op: " +
             AstPrinter::unary_op_to_string(node.op_type) + ")");
      break;
  }

  m_last_expr_operand = dest_reg;
}

void IrVisitor::visit(AssignmentNode& node) {
  node.rvalue->accept(*this);
  IROperand rval_op = m_last_expr_operand;

  // Handle all possible L-values
  if (auto lval_ident =
          std::dynamic_pointer_cast<IdentifierNode>(node.lvalue)) {
    assert(m_vars.count(lval_ident->name) &&
           "Type checker should identify use of undeclared variable");
    IR_Variable lval_var = m_vars.at(lval_ident->name);
    m_ir_gen.emit_assign(lval_var, rval_op);
  } else if (auto array_idx_node =
                 std::dynamic_pointer_cast<ArrayIndexNode>(node.lvalue)) {
    // arr[idx] = rval_op  => STORE target_addr, rval_op
    array_idx_node->object->accept(*this); // base address
    IROperand base_addr_op = m_last_expr_operand;

    array_idx_node->index->accept(*this); // index
    IROperand index_op = m_last_expr_operand;

    assert(node.lvalue->expr_type && "LValue in assignment should have a type");
    IR_Immediate element_size_op(node.lvalue->expr_type->get_byte_size());

    IR_Register offset_reg = m_ir_gen.new_temp_reg();
    m_ir_gen.emit_mul(offset_reg, index_op, element_size_op);

    IR_Register target_addr_reg = m_ir_gen.new_temp_reg();
    m_ir_gen.emit_add(target_addr_reg, base_addr_op, offset_reg);

    m_ir_gen.emit_store(target_addr_reg, rval_op);
  } else if (auto unary_deref_node =
                 std::dynamic_pointer_cast<UnaryExprNode>(node.lvalue)) {
    if (unary_deref_node->op_type == UnaryOperator::Dereference) {
      // *ptr = rval_op => STORE ptr_val, rval_op
      unary_deref_node->operand->accept(*this);
      IROperand ptr_addr_op = m_last_expr_operand;
      m_ir_gen.emit_store(ptr_addr_op, rval_op);
    } else {
      unimpl("AssignmentNode to non-dereference UnaryExpr LValue");
    }
  } else {
    // TODO: MemberAccessNode
    unimpl("AssignmentNode (complex lvalue)");
  }

  m_last_expr_operand = rval_op;
}

void IrVisitor::visit(GroupedExprNode& node) { node.expression->accept(*this); }

// Statement Nodes
void IrVisitor::visit(VariableDeclNode& node) {
  assert(!m_vars.count(node.var_name->name) &&
         "Type checker should identify variable re-declaration");

  const std::string& var_name = node.var_name->name;
  IR_Variable var_operand(var_name);
  m_vars.insert({var_name, var_operand});

  if (node.initializer) {
    node.initializer->accept(*this);
    m_ir_gen.emit_assign(var_operand, m_last_expr_operand);
  } else {
    // default values
    // TODO: update this within Type system
    m_ir_gen.emit_assign(var_operand, IR_Immediate(0));
  }
}

void IrVisitor::visit(ExpressionStatementNode& node) {
  node.expression->accept(*this);
}

void IrVisitor::visit(IfStmtNode& node) {
  node.condition->accept(*this);
  IROperand cond_op = m_last_expr_operand;

  IR_Label else_label = m_ir_gen.new_label();
  IR_Label end_label = m_ir_gen.new_label();

  m_ir_gen.emit_if_z(cond_op, else_label);

  node.then_branch->accept(*this);

  if (node.else_branch) {
    m_ir_gen.emit_goto(end_label);
  }

  m_ir_gen.emit_label(else_label);

  if (node.else_branch) {
    node.else_branch->accept(*this);
    m_ir_gen.emit_label(end_label);
  }
  // else the else_branch serves as the end_label
}

void IrVisitor::visit(WhileStmtNode& node) {
  IR_Label start_loop_label = m_ir_gen.new_label();
  IR_Label end_loop_label = m_ir_gen.new_label();

  m_loop_contexts.push({start_loop_label, end_loop_label});

  m_ir_gen.emit_label(start_loop_label);

  node.condition->accept(*this);
  IROperand cond_op = m_last_expr_operand;
  m_ir_gen.emit_if_z(cond_op, end_loop_label);

  node.body->accept(*this);

  m_ir_gen.emit_goto(start_loop_label);
  m_ir_gen.emit_label(end_loop_label);

  m_loop_contexts.pop();
}

void IrVisitor::visit(BreakStmtNode&) {
  if (m_loop_contexts.empty()) {
    throw std::runtime_error("IR Gen: Break statement outside of loop context");
  }
  m_ir_gen.emit_goto(m_loop_contexts.top().second); // break_target
}

void IrVisitor::visit(ContinueStmtNode&) {
  if (m_loop_contexts.empty()) {
    throw std::runtime_error(
        "IR Gen: Continue statement outside of loop context");
  }
  m_ir_gen.emit_goto(m_loop_contexts.top().first); // continue_target
}

void IrVisitor::visit(BlockNode& node) {
  // save current variable mappings to implement block scope
  std::unordered_map<std::string, IR_Variable> prev_var_operands = m_vars;
  for (const StmtPtr& stmt : node.statements) {
    stmt->accept(*this);
  }
  // restore mappings from before the block
  m_vars = prev_var_operands;
}

void IrVisitor::visit(FunctionDeclNode& node) {
  std::string original_func_name = node.name->name;
  std::string func_ir_name = original_func_name;

  if (original_func_name == "main") {
    m_main_function_defined = true;
  } else {
    func_ir_name = "_" + original_func_name;
  }

  IR_Label func_label = m_ir_gen.new_func_label(func_ir_name);
  m_func_labels.insert({original_func_name, func_label});

  uint64_t stack_size = node.type->get_byte_size();
  m_ir_gen.emit_begin_func(func_label, IR_Immediate(stack_size));

  // save for scoping
  std::unordered_map<std::string, IR_Variable> prev_var_operands = m_vars;
  m_vars.clear();

  // allocate registers for arguments
  for (const ParamPtr& param_node : node.params) {
    param_node->accept(*this);
  }

  if (node.return_type_name.has_value()) {
    const std::string& ret_name = node.return_type_name.value();
    IR_Variable ret_var_op(ret_name);
    m_vars.insert({ret_name, ret_var_op});
  }

  m_emitted_return = false; // reset
  if (node.body) {
    node.body->accept(*this);
  }

  if (!m_emitted_return) {
    m_ir_gen.emit_ret();
  }

  m_ir_gen.emit_end_func();

  // restore var context
  m_vars = prev_var_operands;
}

void IrVisitor::visit(ArgumentNode& node) {
  // Much later on we will be include handling of different forms of copying, vs
  // 'give'ing and 'take'ing. For now, just accept from the expression.
  node.expression->accept(*this);
}
void IrVisitor::visit(ParamNode& node) {
  const std::string& param_name = node.name->name;
  IR_Variable param_var(param_name);
  m_vars.insert({param_name, param_var});
  node.name->expr_type = node.type;
}

void IrVisitor::visit(FunctionCallNode& node) {
  uint64_t total_param_size = 0;

  for (const ArgPtr& arg_node : node.arguments) {
    arg_node->accept(*this);

    m_ir_gen.emit_push_arg(m_last_expr_operand);
    assert(arg_node->expression->expr_type &&
           "Argument expression must have a type");
    total_param_size += arg_node->expression->expr_type->get_byte_size();
  }

  IdentPtr callee_ident =
      std::dynamic_pointer_cast<IdentifierNode>(node.callee);
  if (!callee_ident) {
    unimpl("FunctionCallNode with complex callee expressions (func ptr)");
    return;
  }
  std::string func_name = callee_ident->name;

  assert(m_func_labels.count(func_name));
  IR_Label func_label = m_func_labels.at(func_name);

  std::optional<IR_Register> result_reg_opt = std::nullopt;
  assert(node.expr_type && "Type checker should resolve function call");

  // if it is not void, prepare a temp register for return value
  if (!(node.expr_type->is<Type::Named>() &&
        node.expr_type->as<Type::Named>().identifier == "u0")) {
    IR_Register call_result_reg = m_ir_gen.new_temp_reg();
    result_reg_opt = call_result_reg;
    m_last_expr_operand = call_result_reg;
  } else {
    // void placeholder
    m_last_expr_operand = IR_Immediate(0);
  }

  m_ir_gen.emit_lcall(result_reg_opt, func_label);
  m_ir_gen.emit_pop_args(IR_Immediate(total_param_size));
}

void IrVisitor::visit(ReturnStmtNode& node) {
  m_emitted_return = true;
  if (node.value) {
    node.value->accept(*this);
    m_ir_gen.emit_ret(m_last_expr_operand);
  } else {
    // void
    m_ir_gen.emit_ret();
  }
}

void IrVisitor::visit(ForStmtNode& node) {
  // (initializer; condition; iteration) body
  std::unordered_map<std::string, IR_Variable> prev_var_operands;
  bool new_scope_created = false;

  if (node.initializer.has_value() &&
      std::holds_alternative<StmtPtr>(*node.initializer)) {
    // If initializer is a declaration, it creates a new scope for the loop
    prev_var_operands = m_vars;
    new_scope_created = true;
  }

  if (node.initializer.has_value()) {
    std::visit(
        [this](auto&& arg) {
          if (arg) arg->accept(*this);
        },
        node.initializer.value());
  }

  IR_Label condition_label = m_ir_gen.new_label();
  IR_Label iteration_label = m_ir_gen.new_label();
  IR_Label body_label = m_ir_gen.new_label();
  IR_Label end_loop_label = m_ir_gen.new_label();

  // 'continue': jump to iteration_label.
  // 'break': jump to end_loop_label.
  m_loop_contexts.push({iteration_label, end_loop_label});

  m_ir_gen.emit_label(condition_label);
  if (node.condition) {
    node.condition->accept(*this);
    m_ir_gen.emit_if_z(m_last_expr_operand, end_loop_label);
  }

  m_ir_gen.emit_label(body_label);
  node.body->accept(*this);

  m_ir_gen.emit_label(iteration_label);
  if (node.iteration) {
    node.iteration->accept(*this);
  }
  m_ir_gen.emit_goto(condition_label);

  m_ir_gen.emit_label(end_loop_label);
  m_loop_contexts.pop();

  if (new_scope_created) {
    m_vars = prev_var_operands;
  }
}

static bool is_unsigned_int(std::shared_ptr<Type> type) {
  if (!type->is<Type::Named>()) return false;
  const std::string& n = type->as<Type::Named>().identifier;
  return n == "u8" || n == "u16" || n == "u32" || n == "u64" || n == "bool";
}

static bool is_signed_int(std::shared_ptr<Type> type) {
  if (!type->is<Type::Named>()) return false;
  const std::string& n = type->as<Type::Named>().identifier;
  return n == "i8" || n == "i16" || n == "i32" || n == "i64";
}

void IrVisitor::visit(ReadStmtNode& node) {
  assert(node.expression->expr_type &&
         "Read expression must have a type from typechecker");
  std::shared_ptr<Type> expr_type = node.expression->expr_type;

  IR_Register temp_val_reg = m_ir_gen.new_temp_reg();
  m_ir_gen.emit_lcall(temp_val_reg, IR_Label("read_word"));

  // Convert the word to an int
  if (is_unsigned_int(expr_type)) {
    m_ir_gen.emit_lcall(temp_val_reg, IR_Label("parse_uint"));
  } else {
    assert(is_signed_int(expr_type));
    m_ir_gen.emit_lcall(temp_val_reg, IR_Label("parse_int"));
  }

  if (auto ident_node =
          std::dynamic_pointer_cast<IdentifierNode>(node.expression)) {
    IR_Variable lval_var = m_vars.at(ident_node->name);
    m_ir_gen.emit_assign(lval_var, temp_val_reg);
  } else {
    unimpl("ReadStmtNode with complex l-value");
  }
}

void IrVisitor::visit(PrintStmtNode& node) {
  node.expression->accept(*this);
  IROperand val_op = m_last_expr_operand;

  assert(node.expression->expr_type &&
         "Print expression must have a type from typechecker");
  std::shared_ptr<Type> expr_type = node.expression->expr_type;

  IR_Label print_func_lbl = get_runtime_print_call(expr_type);

  m_ir_gen.emit_push_arg(val_op);
  m_ir_gen.emit_lcall(std::nullopt, print_func_lbl);
  m_ir_gen.emit_pop_args(IR_Immediate(expr_type->get_byte_size()));
}

void IrVisitor::visit(AsmBlockNode& node) {
  m_ir_gen.emit_asm_block(node.body);
}

void IrVisitor::visit(ArrayIndexNode& node) { // R-value access: x = arr[i]
  node.object->accept(*this);                 // base addr
  IROperand base_addr_op = m_last_expr_operand;

  node.index->accept(*this);
  IROperand index_op = m_last_expr_operand;

  assert(node.expr_type &&
         "ArrayIndexNode must have its element type resolved");
  IR_Immediate element_size_op(node.expr_type->get_byte_size());

  IR_Register offset_reg = m_ir_gen.new_temp_reg();
  m_ir_gen.emit_mul(offset_reg, index_op, element_size_op);

  IR_Register target_addr_reg = m_ir_gen.new_temp_reg();
  m_ir_gen.emit_add(target_addr_reg, base_addr_op, offset_reg);

  IR_Register result_val_reg = m_ir_gen.new_temp_reg();
  m_ir_gen.emit_load(result_val_reg, target_addr_reg);
  m_last_expr_operand = result_val_reg;
}

void IrVisitor::visit(FloatLiteralNode&) { unimpl("FloatLiteralNode"); }
void IrVisitor::visit(NullLiteralNode&) { unimpl("NullLiteralNode"); }

void IrVisitor::visit(ErrorStmtNode&) { unimpl("ErrorStmtNode"); }

void IrVisitor::visit(NewExprNode&) { unimpl("NewExprNode"); }
void IrVisitor::visit(FreeStmtNode&) { unimpl("FreeStmtNode"); }

void IrVisitor::visit(StructDeclNode&) { unimpl("StructDeclNode"); }
void IrVisitor::visit(StructFieldNode&) { unimpl("StructFieldNode"); }
void IrVisitor::visit(MemberAccessNode&) { unimpl("MemberAccessNode"); }
void IrVisitor::visit(StructLiteralNode&) { unimpl("StructLiteralNode"); }
void IrVisitor::visit(StructFieldInitializerNode&) { unimpl("FieldInitNode"); }

void IrVisitor::visit(SwitchStmtNode&) { unimpl("SwitchStmtNode"); }
void IrVisitor::visit(CaseNode&) { unimpl("CaseNode"); }

IR_Label IrVisitor::get_runtime_print_call(const std::shared_ptr<Type>& type) {
  assert(type && "Type must be known for print function selection");
  if (type->is<Type::Named>()) {
    const std::string& name = type->as<Type::Named>().identifier;
    if (name == "string") return IR_Label("print_string");
    if (name == "bool") return IR_Label("print_int");
    if (name == "i64" || name == "u64" || name == "i32" || name == "u32" ||
        name == "i16" || name == "u16" || name == "i8" || name == "u8") {
      return IR_Label("print_int");
    }
  }
  return IR_Label("print_int");
}
