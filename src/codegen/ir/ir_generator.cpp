#include "ir_generator.h"

IrGenerator::IrGenerator() : m_next_reg_id(0), m_next_label_id(0) {}

IR_Register IrGenerator::new_temp_reg() { return IR_Register(m_next_reg_id++); }
IR_Label IrGenerator::new_label() { return IR_Label(m_next_label_id++); }
IR_Label IrGenerator::new_func_label(const std::string& func_name) {
  return IR_Label(func_name);
}

void IrGenerator::emit_begin_func(IR_Label func_label) {
  m_instructions.emplace_back(IROpCode::BEGIN_FUNC, func_label,
                              std::vector<IROperand>{});
}

void IrGenerator::emit_end_func(IROperand return_val, uint64_t return_size) {
  m_instructions.emplace_back(IROpCode::END_FUNC, std::nullopt,
                              std::vector<IROperand>{return_val}, return_size);
}

void IrGenerator::emit_end_func() {
  m_instructions.emplace_back(IROpCode::END_FUNC);
}

void IrGenerator::emit_exit() { m_instructions.emplace_back(IROpCode::EXIT); }

void IrGenerator::emit_assign(IROperand dst, IROperand src, uint64_t size) {
  // Dest for ASSIGN must be IR_Register or IR_Variable
  assert(std::holds_alternative<IR_Register>(dst) ||
         std::holds_alternative<IR_Variable>(dst));
  m_instructions.emplace_back(IROpCode::ASSIGN, dst,
                              std::vector<IROperand>{src}, size);
}

void IrGenerator::emit_load(IR_Register dst, IROperand addr_src,
                            uint64_t size) {
  m_instructions.emplace_back(IROpCode::LOAD, dst,
                              std::vector<IROperand>{addr_src}, size);
}

void IrGenerator::emit_store(IROperand address_dest, IROperand src,
                             uint64_t size) {
  m_instructions.emplace_back(IROpCode::STORE, address_dest,
                              std::vector<IROperand>{src}, size);
}

void IrGenerator::emit_add(IR_Register dst, IROperand s1, IROperand s2,
                           uint64_t size) {
  m_instructions.emplace_back(IROpCode::ADD, dst,
                              std::vector<IROperand>{s1, s2}, size);
}

void IrGenerator::emit_sub(IR_Register dst, IROperand s1, IROperand s2,
                           uint64_t size) {
  m_instructions.emplace_back(IROpCode::SUB, dst,
                              std::vector<IROperand>{s1, s2}, size);
}

void IrGenerator::emit_mul(IR_Register dst, IROperand s1, IROperand s2,
                           uint64_t size) {
  m_instructions.emplace_back(IROpCode::MUL, dst,
                              std::vector<IROperand>{s1, s2}, size);
}

void IrGenerator::emit_div(IR_Register dst, IROperand s1, IROperand s2,
                           uint64_t size) {
  m_instructions.emplace_back(IROpCode::DIV, dst,
                              std::vector<IROperand>{s1, s2}, size);
}

void IrGenerator::emit_mod(IR_Register dst, IROperand s1, IROperand s2,
                           uint64_t size) {
  m_instructions.emplace_back(IROpCode::MOD, dst,
                              std::vector<IROperand>{s1, s2}, size);
}

void IrGenerator::emit_neg(IR_Register dst, IROperand src, uint64_t size) {
  m_instructions.emplace_back(IROpCode::NEG, dst, std::vector<IROperand>{src},
                              size);
}

void IrGenerator::emit_log_not(IR_Register dst, IROperand src, uint64_t size) {
  m_instructions.emplace_back(IROpCode::NOT, dst, std::vector<IROperand>{src},
                              size);
}

void IrGenerator::emit_log_and(IR_Register dst, IROperand s1, IROperand s2,
                               uint64_t size) {
  m_instructions.emplace_back(IROpCode::AND, dst,
                              std::vector<IROperand>{s1, s2}, size);
}

void IrGenerator::emit_log_or(IR_Register dst, IROperand s1, IROperand s2,
                              uint64_t size) {
  m_instructions.emplace_back(IROpCode::OR, dst, std::vector<IROperand>{s1, s2},
                              size);
}

void IrGenerator::emit_cmp_eq(IR_Register dst, IROperand s1, IROperand s2,
                              uint64_t size) {
  m_instructions.emplace_back(IROpCode::CMP_EQ, dst,
                              std::vector<IROperand>{s1, s2}, size);
}

void IrGenerator::emit_cmp_ne(IR_Register dst, IROperand s1, IROperand s2,
                              uint64_t size) {
  m_instructions.emplace_back(IROpCode::CMP_NE, dst,
                              std::vector<IROperand>{s1, s2}, size);
}

void IrGenerator::emit_cmp_lt(IR_Register dst, IROperand s1, IROperand s2,
                              uint64_t size) {
  m_instructions.emplace_back(IROpCode::CMP_LT, dst,
                              std::vector<IROperand>{s1, s2}, size);
}

void IrGenerator::emit_cmp_le(IR_Register dst, IROperand s1, IROperand s2,
                              uint64_t size) {
  m_instructions.emplace_back(IROpCode::CMP_LE, dst,
                              std::vector<IROperand>{s1, s2}, size);
}

void IrGenerator::emit_cmp_gt(IR_Register dst, IROperand s1, IROperand s2,
                              uint64_t size) {
  m_instructions.emplace_back(IROpCode::CMP_GT, dst,
                              std::vector<IROperand>{s1, s2}, size);
}

void IrGenerator::emit_cmp_ge(IR_Register dst, IROperand s1, IROperand s2,
                              uint64_t size) {
  m_instructions.emplace_back(IROpCode::CMP_GE, dst,
                              std::vector<IROperand>{s1, s2}, size);
}

void IrGenerator::emit_label(IR_Label label) {
  m_instructions.emplace_back(IROpCode::LABEL, label, std::vector<IROperand>{});
}

void IrGenerator::emit_goto(IR_Label target) {
  m_instructions.emplace_back(IROpCode::GOTO, std::nullopt,
                              std::vector<IROperand>{target});
}

void IrGenerator::emit_if_z(IROperand cond, IR_Label target,
                            uint64_t cond_operands_size) {
  m_instructions.emplace_back(IROpCode::IF_Z, std::nullopt,
                              std::vector<IROperand>{cond, target},
                              cond_operands_size);
}

void IrGenerator::emit_push_arg(IROperand src, uint64_t size) {
  m_instructions.emplace_back(IROpCode::PUSH_ARG, std::nullopt,
                              std::vector<IROperand>{src}, size);
}

void IrGenerator::emit_pop_args() {
  m_instructions.emplace_back(IROpCode::POP_ARGS);
}

void IrGenerator::emit_lcall(std::optional<IR_Register> dst,
                             IR_Label func_target, uint64_t return_size) {
  m_instructions.emplace_back(IROpCode::LCALL, dst,
                              std::vector<IROperand>{func_target}, return_size);
}

void IrGenerator::emit_asm_block(const std::string& asm_code) {
  m_instructions.emplace_back(IROpCode::ASM_BLOCK, std::nullopt,
                              std::vector<IROperand>{asm_code});
}

const std::vector<IRInstruction>& IrGenerator::get_instructions() const {
  return m_instructions;
}
