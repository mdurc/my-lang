#include "ir_generator.h"

IrGenerator::IrGenerator() : m_next_reg_id(0), m_next_label_id(0) {}

IR_Register IrGenerator::new_temp_reg() { return IR_Register(m_next_reg_id++); }
IR_Label IrGenerator::new_label() { return IR_Label(m_next_label_id++); }
IR_Label IrGenerator::new_func_label(const std::string& func_name) {
  return IR_Label(func_name);
}

void IrGenerator::emit_begin_func(IR_Label func_label,
                                  IR_Immediate stack_size) {
  m_instructions.emplace_back(IROpCode::BEGIN_FUNC, func_label,
                              std::vector<IROperand>{stack_size});
}

void IrGenerator::emit_end_func() {
  m_instructions.emplace_back(IROpCode::END_FUNC);
}

void IrGenerator::emit_exit() { m_instructions.emplace_back(IROpCode::EXIT); }

void IrGenerator::emit_assign(IROperand dest, IROperand src) {
  // Dest for ASSIGN must be IR_Register or IR_Variable
  assert(std::holds_alternative<IR_Register>(dest) ||
         std::holds_alternative<IR_Variable>(dest));
  m_instructions.emplace_back(IROpCode::ASSIGN, dest,
                              std::vector<IROperand>{src});
}

void IrGenerator::emit_load(IR_Register dest, IROperand addr_src) {
  m_instructions.emplace_back(IROpCode::LOAD, dest,
                              std::vector<IROperand>{addr_src});
}

void IrGenerator::emit_store(IROperand address_dest, IROperand src) {
  m_instructions.emplace_back(IROpCode::STORE, address_dest,
                              std::vector<IROperand>{src});
}

void IrGenerator::emit_add(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::ADD, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_sub(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::SUB, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_mul(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::MUL, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_div(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::DIV, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_mod(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::MOD, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_neg(IR_Register dest, IROperand src) {
  m_instructions.emplace_back(IROpCode::NEG, dest, std::vector<IROperand>{src});
}

void IrGenerator::emit_logical_not(IR_Register dest, IROperand src) {
  m_instructions.emplace_back(IROpCode::NOT, dest, std::vector<IROperand>{src});
}

void IrGenerator::emit_logical_and(IR_Register dest, IROperand src1,
                                   IROperand src2) {
  m_instructions.emplace_back(IROpCode::AND, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_logical_or(IR_Register dest, IROperand src1,
                                  IROperand src2) {
  m_instructions.emplace_back(IROpCode::OR, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_cmp_eq(IR_Register dest, IROperand src1,
                              IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_EQ, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_cmp_ne(IR_Register dest, IROperand src1,
                              IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_NE, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_cmp_lt(IR_Register dest, IROperand src1,
                              IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_LT, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_cmp_le(IR_Register dest, IROperand src1,
                              IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_LE, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_cmp_gt(IR_Register dest, IROperand src1,
                              IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_GT, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_cmp_ge(IR_Register dest, IROperand src1,
                              IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_GE, dest,
                              std::vector<IROperand>{src1, src2});
}

void IrGenerator::emit_label(IR_Label label) {
  m_instructions.emplace_back(IROpCode::LABEL, label, std::vector<IROperand>{});
}

void IrGenerator::emit_goto(IR_Label target) {
  m_instructions.emplace_back(IROpCode::GOTO, std::nullopt,
                              std::vector<IROperand>{target});
}

void IrGenerator::emit_if_z(IROperand cond, IR_Label target) {
  m_instructions.emplace_back(IROpCode::IF_Z, std::nullopt,
                              std::vector<IROperand>{cond, target});
}

void IrGenerator::emit_push_param(IROperand src) {
  m_instructions.emplace_back(IROpCode::PUSH_PARAM, std::nullopt,
                              std::vector<IROperand>{src});
}

void IrGenerator::emit_pop_params(IR_Immediate num_bytes) {
  m_instructions.emplace_back(IROpCode::POP_PARAMS, std::nullopt,
                              std::vector<IROperand>{num_bytes});
}

void IrGenerator::emit_lcall(std::optional<IR_Register> dest,
                             IR_Label func_target) {
  if (dest.has_value()) {
    m_instructions.emplace_back(IROpCode::LCALL, dest.value(),
                                std::vector<IROperand>{func_target});
  } else {
    m_instructions.emplace_back(IROpCode::LCALL, std::nullopt,
                                std::vector<IROperand>{func_target});
  }
}

void IrGenerator::emit_ret() {
  m_instructions.emplace_back(IROpCode::RETURN); // void return
}

void IrGenerator::emit_ret(IROperand retval) {
  m_instructions.emplace_back(IROpCode::RETURN, std::nullopt,
                              std::vector<IROperand>{retval});
}

void IrGenerator::emit_asm_block(const std::string& asm_code) {
  m_instructions.emplace_back(IROpCode::ASM_BLOCK, std::nullopt,
                              std::vector<IROperand>{asm_code});
}

const std::vector<IRInstruction>& IrGenerator::get_instructions() const {
  return m_instructions;
}
