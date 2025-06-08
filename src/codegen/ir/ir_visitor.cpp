#include "ir_visitor.h"

#include <cstdint>

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
  // 8 bytes for integer literal as of now
  m_ir_gen.emit_assign(temp_reg, IR_Immediate(node.value, 8), 8);
  m_last_expr_operand = temp_reg;
}

void IrVisitor::visit(BoolLiteralNode& node) {
  IR_Register temp_reg = m_ir_gen.new_temp_reg();
  // 1 byte for boolean literal as of now
  m_ir_gen.emit_assign(temp_reg, IR_Immediate(node.value ? 1 : 0, 1), 1);
  m_last_expr_operand = temp_reg;
}

void IrVisitor::visit(StringLiteralNode& node) {
  // backend handles putting string in memory
  IR_Register temp_reg = m_ir_gen.new_temp_reg();
  // Ptr size for string literal
  m_ir_gen.emit_assign(temp_reg, node.value, Type::PTR_SIZE);
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

  // the only case that these should/can be different sizes is if they are nums
  uint64_t l_size = node.left->expr_type->get_byte_size();
  uint64_t r_size = node.right->expr_type->get_byte_size();
  uint64_t size = std::min(l_size, r_size);

  switch (node.op_type) {
    case BinOperator::Plus:
      m_ir_gen.emit_add(dest_reg, left_op, right_op, size);
      break;
    case BinOperator::Minus:
      m_ir_gen.emit_sub(dest_reg, left_op, right_op, size);
      break;
    case BinOperator::Multiply:
      m_ir_gen.emit_mul(dest_reg, left_op, right_op, size);
      break;
    case BinOperator::Divide:
      m_ir_gen.emit_div(dest_reg, left_op, right_op, size);
      break;
    case BinOperator::Equal:
      m_ir_gen.emit_cmp_eq(dest_reg, left_op, right_op, size);
      break;
    case BinOperator::NotEqual:
      m_ir_gen.emit_cmp_ne(dest_reg, left_op, right_op, size);
      break;
    case BinOperator::LessThan:
      m_ir_gen.emit_cmp_lt(dest_reg, left_op, right_op, size);
      break;
    case BinOperator::LessEqual:
      m_ir_gen.emit_cmp_le(dest_reg, left_op, right_op, size);
      break;
    case BinOperator::GreaterThan:
      m_ir_gen.emit_cmp_gt(dest_reg, left_op, right_op, size);
      break;
    case BinOperator::GreaterEqual:
      m_ir_gen.emit_cmp_ge(dest_reg, left_op, right_op, size);
      break;
    case BinOperator::LogicalAnd:
      m_ir_gen.emit_log_and(dest_reg, left_op, right_op, size);
      break;
    case BinOperator::LogicalOr:
      m_ir_gen.emit_log_or(dest_reg, left_op, right_op, size);
      break;
    case BinOperator::Modulo:
      m_ir_gen.emit_mod(dest_reg, left_op, right_op, size);
      break;
  }

  m_last_expr_operand = dest_reg;
}

void IrVisitor::visit(UnaryExprNode& node) {
  node.operand->accept(*this);
  IROperand operand_op = m_last_expr_operand;

  IR_Register dest_reg = m_ir_gen.new_temp_reg();

  uint64_t size = node.operand->expr_type->get_byte_size();
  switch (node.op_type) {
    case UnaryOperator::Negate:
      // dest = -operand
      m_ir_gen.emit_neg(dest_reg, operand_op, size);
      break;
    case UnaryOperator::LogicalNot:
      m_ir_gen.emit_log_not(dest_reg, operand_op, size);
      break;
    case UnaryOperator::Dereference:
      m_ir_gen.emit_load(dest_reg, operand_op, size);
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
  uint64_t size = node.lvalue->expr_type->get_byte_size();

  // Handle all possible L-values
  if (auto lval_ident =
          std::dynamic_pointer_cast<IdentifierNode>(node.lvalue)) {
    assert(m_vars.count(lval_ident->name) &&
           "Type checker should identify use of undeclared variable");
    IR_Variable lval_var = m_vars.at(lval_ident->name);
    m_ir_gen.emit_assign(lval_var, rval_op, size);
  } else if (auto array_idx_node =
                 std::dynamic_pointer_cast<ArrayIndexNode>(node.lvalue)) {
    // arr[idx] = rval_op  => STORE target_addr, rval_op
    array_idx_node->object->accept(*this); // base address
    IROperand base_addr_op = m_last_expr_operand;

    array_idx_node->index->accept(*this); // index
    IROperand index_op = m_last_expr_operand;

    assert(node.lvalue->expr_type && "LValue in assignment should have a type");
    IR_Immediate idx_offset(node.lvalue->expr_type->get_byte_size(), 8);

    IR_Register offset_reg = m_ir_gen.new_temp_reg();
    m_ir_gen.emit_mul(offset_reg, index_op, idx_offset, size);

    IR_Register target_addr_reg = m_ir_gen.new_temp_reg();
    m_ir_gen.emit_add(target_addr_reg, base_addr_op, offset_reg, size);

    m_ir_gen.emit_store(target_addr_reg, rval_op, size);
  } else if (auto unary_deref_node =
                 std::dynamic_pointer_cast<UnaryExprNode>(node.lvalue)) {
    if (unary_deref_node->op_type == UnaryOperator::Dereference) {
      // *ptr = rval_op => STORE ptr_val, rval_op
      unary_deref_node->operand->accept(*this);
      IROperand ptr_addr_op = m_last_expr_operand;
      m_ir_gen.emit_store(ptr_addr_op, rval_op, size);
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
  uint64_t size = node.type->get_byte_size();
  IR_Variable var_operand(var_name, size);
  m_vars.insert({var_name, var_operand});

  if (node.initializer) {
    node.initializer->accept(*this);
    m_ir_gen.emit_assign(var_operand, m_last_expr_operand, size);
  } else {
    // default values, right now just assign 0
    m_ir_gen.emit_assign(var_operand, IR_Immediate(0, 8), size);
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

  m_ir_gen.emit_if_z(cond_op, else_label,
                     node.condition->expr_type->get_byte_size());

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
  m_ir_gen.emit_if_z(cond_op, end_loop_label,
                     node.condition->expr_type->get_byte_size());

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

  m_ir_gen.emit_begin_func(func_label);

  // save for scoping
  std::unordered_map<std::string, IR_Variable> prev_var_operands = m_vars;
  m_vars.clear();

  // allocate registers for arguments
  std::vector<IR_Variable> ordered_param_vars;
  for (const ParamPtr& param_node : node.params) {
    param_node->accept(*this); // inserts into m_vars
    ordered_param_vars.push_back(m_vars.at(param_node->name->name));
  }

  // emit assignments from argument slots to parameter variables so that the
  // code generation can load the passed arguments onto the stack as local vars
  size_t p_amt = ordered_param_vars.size();
  for (size_t i = 0; i < p_amt; ++i) {
    const IR_Variable& param_var = ordered_param_vars[i];
    uint64_t param_size = node.params[i]->type->get_byte_size();
    m_ir_gen.emit_assign(param_var, IR_ParameterSlot(i, p_amt, param_size),
                         param_size);
  }

  if (node.return_type_name.has_value()) {
    const std::string& ret_name = node.return_type_name.value();
    IR_Variable ret_var_op(ret_name, node.return_type->get_byte_size());
    m_vars.insert({ret_name, ret_var_op});
  }

  m_emitted_return = false; // reset
  if (node.body) {
    node.body->accept(*this);
  }

  if (!m_emitted_return) {
    m_ir_gen.emit_end_func();
  }

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
  IR_Variable param_var(param_name, node.type->get_byte_size());
  m_vars.insert({param_name, param_var});
  node.name->expr_type = node.type;
}

void IrVisitor::visit(FunctionCallNode& node) {
  for (const ArgPtr& arg_node : node.arguments) {
    arg_node->accept(*this);

    m_ir_gen.emit_push_arg(m_last_expr_operand,
                           arg_node->expression->expr_type->get_byte_size());
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
    m_last_expr_operand = IR_Immediate(0, 8);
  }

  m_ir_gen.emit_lcall(result_reg_opt, func_label,
                      node.expr_type->get_byte_size());
  m_ir_gen.emit_pop_args();
}

void IrVisitor::visit(ReturnStmtNode& node) {
  m_emitted_return = true;
  if (node.value) {
    node.value->accept(*this);
    m_ir_gen.emit_end_func(m_last_expr_operand,
                           node.value->expr_type->get_byte_size());
  } else {
    // void
    m_ir_gen.emit_end_func();
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
    m_ir_gen.emit_if_z(m_last_expr_operand, end_loop_label,
                       node.condition->expr_type->get_byte_size());
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
  m_ir_gen.emit_lcall(temp_val_reg, IR_Label("read_word"), Type::PTR_SIZE);

  // Convert the word to an int
  if (is_unsigned_int(expr_type)) {
    m_ir_gen.emit_lcall(temp_val_reg, IR_Label("parse_uint"), 8);
  } else if (is_signed_int(expr_type)) {
    m_ir_gen.emit_lcall(temp_val_reg, IR_Label("parse_int"), 8);
  }

  if (auto ident_node =
          std::dynamic_pointer_cast<IdentifierNode>(node.expression)) {
    IR_Variable lval_var = m_vars.at(ident_node->name);
    m_ir_gen.emit_assign(lval_var, temp_val_reg, expr_type->get_byte_size());
  } else {
    unimpl("ReadStmtNode with complex l-value");
  }
}

void IrVisitor::visit(PrintStmtNode& node) {
  for (size_t i = 0; i < node.expressions.size(); ++i) {
    node.expressions[i]->accept(*this);
    IROperand val_op = m_last_expr_operand;

    assert(node.expressions[i]->expr_type &&
           "Print expression must have a type from typechecker");
    std::shared_ptr<Type> expr_type = node.expressions[i]->expr_type;

    IR_Label print_func_lbl = get_runtime_print_call(expr_type);

    m_ir_gen.emit_push_arg(val_op, expr_type->get_byte_size());
    m_ir_gen.emit_lcall(std::nullopt, print_func_lbl, 0);
    m_ir_gen.emit_pop_args();
  }
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
  uint64_t size = node.expr_type->get_byte_size();
  IR_Immediate size_op(size, size);

  IR_Register offset_reg = m_ir_gen.new_temp_reg();
  m_ir_gen.emit_mul(offset_reg, index_op, size_op, size);

  IR_Register target_addr_reg = m_ir_gen.new_temp_reg();
  m_ir_gen.emit_add(target_addr_reg, base_addr_op, offset_reg, size);

  IR_Register result_val_reg = m_ir_gen.new_temp_reg();
  m_ir_gen.emit_load(result_val_reg, target_addr_reg, size);
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
