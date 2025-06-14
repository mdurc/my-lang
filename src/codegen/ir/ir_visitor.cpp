#include "ir_visitor.h"

#include <cstdint>

#include "../../parser/visitor.h"

static bool is_str_cmp(std::shared_ptr<Type> a, std::shared_ptr<Type> b) {
  return a->is<Type::Named>() && a->as<Type::Named>().identifier == "string" &&
         b->is<Type::Named>() && b->as<Type::Named>().identifier == "string";
}

IrVisitor::IrVisitor(const SymTab* tab)
    : m_symtab(tab),
      m_last_expr_operand(IR_Register(-1)),
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

bool IrVisitor::var_exists(const std::string& name, size_t scope_id) {
  std::shared_ptr<Variable> var = m_symtab->lookup_variable(name, scope_id);
  assert(var != nullptr && "var should exist within symbol table");
  uint64_t size = var->type->get_byte_size();
  return m_vars.find(IR_Variable(name + "_" + std::to_string(var->scope_id),
                                 size)) != m_vars.end();
}

const IR_Variable& IrVisitor::get_var(const std::string& name,
                                      size_t scope_id) {
  std::shared_ptr<Variable> var = m_symtab->lookup_variable(name, scope_id);
  assert(var != nullptr && "var should exist within symbol table");
  uint64_t size = var->type->get_byte_size();
  auto itr = m_vars.find(
      IR_Variable(name + "_" + std::to_string(var->scope_id), size));
  assert(itr != m_vars.end());
  return *itr;
}

const IR_Variable* IrVisitor::add_var(const std::string& name, size_t scope_id,
                                      bool is_func_decl = false) {
  std::shared_ptr<Variable> var = m_symtab->lookup_variable(name, scope_id);
  assert(var != nullptr && "var should exist within symbol table");
  uint64_t size = var->type->get_byte_size();
  auto itr = m_vars.insert(IR_Variable(
      name + "_" + std::to_string(var->scope_id), size, is_func_decl));
  return &(*itr.first);
}

void IrVisitor::unimpl(const std::string& note) {
  throw std::runtime_error("Unimplemented IR feature: " + note);
}

// Expression Nodes
void IrVisitor::visit(NullLiteralNode&) {
  // represented by zero
  IR_Register temp_reg = m_ir_gen.new_temp_reg();
  m_ir_gen.emit_assign(temp_reg, IR_Immediate(0, Type::PTR_SIZE),
                       Type::PTR_SIZE);
  m_last_expr_operand = temp_reg;
}

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
  assert(var_exists(node.name, node.scope_id) &&
         "Type checker should identifier use of undeclared identifiers");
  m_last_expr_operand = get_var(node.name, node.scope_id);
}

void IrVisitor::visit(BinaryOpExprNode& node) {
  node.left->accept(*this);
  IROperand left_op = m_last_expr_operand;

  node.right->accept(*this);
  IROperand right_op = m_last_expr_operand;

  IR_Register dest_reg = m_ir_gen.new_temp_reg();

  // the only case that these should/can be different sizes is if they are nums
  assert(node.left->expr_type && node.right->expr_type &&
         "Types should be resolved by Typechecker");
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
      if (is_str_cmp(node.left->expr_type, node.right->expr_type)) {
        // strings are essentially just pointers
        m_ir_gen.emit_cmp_str_eq(dest_reg, left_op, right_op, Type::PTR_SIZE);
      } else {
        m_ir_gen.emit_cmp_eq(dest_reg, left_op, right_op, size);
      }
      break;
    case BinOperator::NotEqual:
      if (is_str_cmp(node.left->expr_type, node.right->expr_type)) {
        m_ir_gen.emit_cmp_str_eq(dest_reg, left_op, right_op, Type::PTR_SIZE);
        m_ir_gen.emit_log_not(dest_reg, dest_reg, 1); // one bit for str_eqs
      } else {
        m_ir_gen.emit_cmp_ne(dest_reg, left_op, right_op, size);
      }
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

void IrVisitor::visit_addrof(const ExprPtr& op, const IR_Register& dst) {
  if (auto ident_node = std::dynamic_pointer_cast<IdentifierNode>(op)) {
    m_ir_gen.emit_addr_of(dst, get_var(ident_node->name, ident_node->scope_id));
  } else if (auto array_idx_node =
                 std::dynamic_pointer_cast<ArrayIndexNode>(op)) {
    // calculate address of arr[idx] and store in dest_reg
    array_idx_node->object->accept(*this);
    IROperand base_addr_op = m_last_expr_operand;

    array_idx_node->index->accept(*this);
    IROperand index_op = m_last_expr_operand;

    assert(array_idx_node->expr_type && array_idx_node->index->expr_type &&
           "node and index should have a resolved type so that we can size it");
    uint64_t pointee_type_size = array_idx_node->expr_type->get_byte_size();
    uint64_t idx_type_size = array_idx_node->index->expr_type->get_byte_size();
    IR_Immediate elem_size_imm(pointee_type_size, idx_type_size);

    IR_Register offset_reg = m_ir_gen.new_temp_reg();
    m_ir_gen.emit_mul(offset_reg, index_op, elem_size_imm, idx_type_size);

    m_ir_gen.emit_add(dst, base_addr_op, offset_reg, idx_type_size);
  } else if (auto unary_deref_node =
                 std::dynamic_pointer_cast<UnaryExprNode>(op)) {
    if (unary_deref_node->op_type == UnaryOperator::Dereference) {
      unary_deref_node->operand->accept(*this);
      m_ir_gen.emit_assign(dst, m_last_expr_operand, Type::PTR_SIZE);
    } else {
      unimpl("AddressOf non-dereference UnaryExpr for lvalue: " +
             AstPrinter::unary_op_to_string(unary_deref_node->op_type));
    }
  } else {
    // TODO MemberAccessNode for &struct.field
    unimpl("AddressOf complex l-value");
  }
}

void IrVisitor::visit(UnaryExprNode& node) {
  node.operand->accept(*this);
  IROperand operand_op = m_last_expr_operand;

  IR_Register dest_reg = m_ir_gen.new_temp_reg();

  assert(node.operand->expr_type && "Types should be resolved by Typechecker");
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
      m_ir_gen.emit_load(dest_reg, operand_op, Type::PTR_SIZE);
      break;
    case UnaryOperator::AddressOf:
    case UnaryOperator::AddressOfMut:
      visit_addrof(node.operand, dest_reg);
      break;
  }

  m_last_expr_operand = dest_reg;
}

void IrVisitor::visit(AssignmentNode& node) {
  node.rvalue->accept(*this);
  IROperand rval_op = m_last_expr_operand;
  assert(node.lvalue->expr_type && "Types should be resolved by Typechecker");
  uint64_t size = node.lvalue->expr_type->get_byte_size();

  // Handle all possible L-values
  if (auto lval_ident =
          std::dynamic_pointer_cast<IdentifierNode>(node.lvalue)) {
    assert(var_exists(lval_ident->name, lval_ident->scope_id) &&
           "Type checker should identify use of undeclared variable");
    IR_Variable lval_var = get_var(lval_ident->name, lval_ident->scope_id);
    m_ir_gen.emit_assign(lval_var, rval_op, size);
  } else if (auto array_idx_node =
                 std::dynamic_pointer_cast<ArrayIndexNode>(node.lvalue)) {
    // arr[idx] = rval_op  => STORE target_addr, rval_op
    array_idx_node->object->accept(*this); // base address
    IROperand base_addr_op = m_last_expr_operand;

    array_idx_node->index->accept(*this); // index
    IROperand index_op = m_last_expr_operand;

    IR_Immediate idx_offset(size, 8);

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
      throw std::runtime_error(
          "IR Error: AssignmentNode to non-dereference UnaryExpr LValue");
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
  assert(node.scope_id == node.var_name->scope_id);
  assert(!var_exists(node.var_name->name, node.scope_id) &&
         "Type checker should identify variable re-declaration");

  const std::string& var_name = node.var_name->name;
  const IR_Variable* var_operand = add_var(var_name, node.scope_id);

  uint64_t size = node.type->get_byte_size();
  if (node.initializer) {
    node.initializer->accept(*this);
    m_ir_gen.emit_assign(*var_operand, m_last_expr_operand, size);
  } else {
    // default values, right now just assign 0
    m_ir_gen.emit_assign(*var_operand, IR_Immediate(0, 8), size);
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

  assert(node.condition->expr_type &&
         "Types should be resolved by Typechecker");
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

  assert(node.condition->expr_type &&
         "Types should be resolved by Typechecker");
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
  for (const StmtPtr& stmt : node.statements) {
    stmt->accept(*this);
  }
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
  add_var(original_func_name, node.scope_id, true);

  m_ir_gen.emit_begin_func(func_label);

  // allocate registers for arguments
  std::vector<IR_Variable> ordered_param_vars;
  for (const ParamPtr& param_node : node.params) {
    param_node->accept(*this); // inserts into m_vars
    ordered_param_vars.push_back(
        get_var(param_node->name->name, param_node->scope_id));
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
    add_var(ret_name, node.body->scope_id);
  }

  m_emitted_return = false; // reset
  if (node.body) {
    node.body->accept(*this);
  }

  if (!m_emitted_return) {
    m_ir_gen.emit_end_func();
  }
}

void IrVisitor::visit(ArgumentNode& node) {
  // Much later on we will be include handling of different forms of copying, vs
  // 'give'ing and 'take'ing. For now, just accept from the expression.
  node.expression->accept(*this);
}

void IrVisitor::visit(ParamNode& node) {
  const std::string& param_name = node.name->name;
  add_var(param_name, node.scope_id);
}

void IrVisitor::visit(FunctionCallNode& node) {
  for (const ArgPtr& arg_node : node.arguments) {
    arg_node->accept(*this);

    assert(arg_node->expression->expr_type &&
           "Types should be resolved by Typechecker");
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

  assert(var_exists(func_name, node.scope_id));
  const IROperand& func_target = get_var(func_name, node.scope_id);

  std::optional<IR_Register> result_reg_opt = std::nullopt;

  // if it is not void, prepare a temp register for return value
  assert(node.expr_type && "Type checker should resolve function call");
  if (!(node.expr_type->is<Type::Named>() &&
        node.expr_type->as<Type::Named>().identifier == "u0")) {
    IR_Register call_result_reg = m_ir_gen.new_temp_reg();
    result_reg_opt = call_result_reg;
    m_last_expr_operand = call_result_reg;
  } else {
    // void placeholder
    m_last_expr_operand = IR_Immediate(0, 8);
  }

  m_ir_gen.emit_lcall(result_reg_opt, func_target,
                      node.expr_type->get_byte_size());
  m_ir_gen.emit_pop_args();
}

void IrVisitor::visit(ReturnStmtNode& node) {
  m_emitted_return = true;
  if (node.value) {
    node.value->accept(*this);
    assert(node.value->expr_type && "Types should be resolved by Typechecker");
    m_ir_gen.emit_end_func(m_last_expr_operand,
                           node.value->expr_type->get_byte_size());
  } else {
    // void
    m_ir_gen.emit_end_func();
  }
}

void IrVisitor::visit(ForStmtNode& node) {
  // (initializer; condition; iteration) body
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
    assert(node.condition->expr_type &&
           "Types should be resolved by Typechecker");
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
  std::shared_ptr<Type> expr_type = node.expression->expr_type;
  assert(expr_type && "Read expression must have a type from typechecker");

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
    IR_Variable lval_var = get_var(ident_node->name, ident_node->scope_id);
    m_ir_gen.emit_assign(lval_var, temp_val_reg, expr_type->get_byte_size());
  } else {
    unimpl("ReadStmtNode with complex l-value");
  }
}

void IrVisitor::visit(PrintStmtNode& node) {
  for (size_t i = 0; i < node.expressions.size(); ++i) {
    node.expressions[i]->accept(*this);
    IROperand val_op = m_last_expr_operand;

    std::shared_ptr<Type> expr_type = node.expressions[i]->expr_type;
    assert(expr_type && "Print expression must have a type from typechecker");

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

void IrVisitor::visit(ExitStmtNode& node) {
  m_ir_gen.emit_exit(node.exit_code);
}

void IrVisitor::visit(SwitchStmtNode& node) {
  node.expression->accept(*this);
  IROperand switch_op = m_last_expr_operand;
  assert(node.expression->expr_type && "Switch expression must have a type");
  uint64_t expr_size = node.expression->expr_type->get_byte_size();

  CasePtr default_case_node = nullptr;
  IR_Label default_body_label;

  std::vector<std::pair<CasePtr, IR_Label>> case_blocks;

  for (const CasePtr& case_ptr : node.cases) {
    if (case_ptr->value) {
      // it is a non-default case
      IR_Label body_label = m_ir_gen.new_label();
      case_blocks.push_back(std::make_pair(case_ptr, body_label));
    } else {
      // the default case
      default_case_node = case_ptr;
      default_body_label = m_ir_gen.new_label();
    }
  }

  IR_Label end_switch_label = m_ir_gen.new_label();
  IR_Label final_fallthrough_label =
      default_case_node ? default_body_label : end_switch_label;

  // emit the cases
  for (const auto& [case_ptr, body_label] : case_blocks) {
    case_ptr->value->accept(*this);
    IROperand case_val_op = m_last_expr_operand;
    IR_Register cmp_reg = m_ir_gen.new_temp_reg();

    // do a not-equal comparison so that we can jump to the case label faster
    m_ir_gen.emit_cmp_ne(cmp_reg, switch_op, case_val_op, expr_size);

    // jump to case label if it was not (not-equal)
    m_ir_gen.emit_if_z(cmp_reg, body_label, expr_size);
  }

  // goto either default case (if one exists) or the exit
  m_ir_gen.emit_goto(final_fallthrough_label);

  for (const auto& [case_ptr, body_label] : case_blocks) {
    m_ir_gen.emit_label(body_label);
    case_ptr->body->accept(*this);
    m_ir_gen.emit_goto(end_switch_label);
  }

  if (default_case_node) {
    m_ir_gen.emit_label(default_body_label);
    default_case_node->body->accept(*this);
    // no need to emit_goto(end_switch_label) because we can just fall-through
  }

  m_ir_gen.emit_label(end_switch_label);
}
void IrVisitor::visit(NewExprNode& node) {
  assert(node.type_to_allocate && "NewExprNode must have a type to allocate");
  assert(node.expr_type && node.expr_type->is<Type::Pointer>() &&
         "NewExprNode result must be a pointer type");

  IR_Register result_ptr_reg = m_ir_gen.new_temp_reg();

  if (node.is_array_allocation) {
    // new <T>[size_expr]
    assert(node.allocation_specifier &&
           "Array allocation must have a size specifier");
    node.allocation_specifier->accept(*this);
    IROperand size_op = m_last_expr_operand;

    assert(node.allocation_specifier->expr_type &&
           "Array size specifier must have a type");
    uint64_t size_op_type_size =
        node.allocation_specifier->expr_type->get_byte_size();

    m_ir_gen.emit_alloc_array(result_ptr_reg,
                              node.type_to_allocate->get_byte_size(), size_op,
                              size_op_type_size);
  } else {
    // new <T>(init_expr) or new <T>()
    std::optional<IROperand> init_op_opt = std::nullopt;
    uint64_t initializer_type_size = 0;

    if (node.allocation_specifier) {
      node.allocation_specifier->accept(*this);
      init_op_opt = m_last_expr_operand;
      assert(node.allocation_specifier->expr_type &&
             "Initializer for new must have a type");
      initializer_type_size =
          node.allocation_specifier->expr_type->get_byte_size();
    }
    m_ir_gen.emit_alloc(result_ptr_reg, node.type_to_allocate->get_byte_size(),
                        init_op_opt, initializer_type_size);
  }
  m_last_expr_operand = result_ptr_reg;
}

void IrVisitor::visit(FreeStmtNode& node) {
  node.expression->accept(*this);
  IROperand ptr_to_free_op = m_last_expr_operand;
  // right now we treat `node.is_array_deallocation` and normal, the same
  m_ir_gen.emit_free(ptr_to_free_op);
}

void IrVisitor::visit(FloatLiteralNode&) { unimpl("FloatLiteralNode"); }
void IrVisitor::visit(ErrorStmtNode&) { unimpl("ErrorStmtNode"); }
void IrVisitor::visit(StructDeclNode&) { unimpl("StructDeclNode"); }
void IrVisitor::visit(StructFieldNode&) { unimpl("StructFieldNode"); }
void IrVisitor::visit(MemberAccessNode&) { unimpl("MemberAccessNode"); }
void IrVisitor::visit(StructLiteralNode&) { unimpl("StructLiteralNode"); }
void IrVisitor::visit(StructFieldInitializerNode&) { unimpl("FieldInitNode"); }
