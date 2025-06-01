#include "ir_generator.h"

IrGenerator::IrGenerator() : m_next_reg_id(0), m_next_label_id(0) {}

IR_Register IrGenerator::new_register() { return IR_Register(m_next_reg_id++); }
IR_Label IrGenerator::new_label() { return IR_Label(m_next_label_id++); }

void IrGenerator::emit_add(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::ADD, dest, src1, src2);
}

void IrGenerator::emit_sub(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::SUB, dest, src1, src2);
}

void IrGenerator::emit_mul(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::MUL, dest, src1, src2);
}

void IrGenerator::emit_div(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::DIV, dest, src1, src2);
}

void IrGenerator::emit_mov(IR_Register dest, IROperand src) {
  m_instructions.emplace_back(IROpCode::MOV, dest, src);
}

void IrGenerator::emit_neg(IR_Register dest, IROperand src) {
  m_instructions.emplace_back(IROpCode::NEG, dest, src);
}

void IrGenerator::emit_logical_not(IR_Register dest, IROperand src) {
  m_instructions.emplace_back(IROpCode::NOT, dest, src);
}

void IrGenerator::emit_cmp_eq(IR_Register dest, IROperand src1,
                              IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_EQ, dest, src1, src2);
}

void IrGenerator::emit_cmp_ne(IR_Register dest, IROperand src1,
                              IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_NE, dest, src1, src2);
}

void IrGenerator::emit_cmp_lt(IR_Register dest, IROperand src1,
                              IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_LT, dest, src1, src2);
}

void IrGenerator::emit_cmp_le(IR_Register dest, IROperand src1,
                              IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_LE, dest, src1, src2);
}

void IrGenerator::emit_cmp_gt(IR_Register dest, IROperand src1,
                              IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_GT, dest, src1, src2);
}

void IrGenerator::emit_cmp_ge(IR_Register dest, IROperand src1,
                              IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_GE, dest, src1, src2);
}
void IrGenerator::emit_logical_and(IR_Register dest, IROperand src1,
                                   IROperand src2) {
  m_instructions.emplace_back(IROpCode::AND, dest, src1, src2);
}

void IrGenerator::emit_logical_or(IR_Register dest, IROperand src1,
                                  IROperand src2) {
  m_instructions.emplace_back(IROpCode::OR, dest, src1, src2);
}

void IrGenerator::emit_mod(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::MOD, dest, src1, src2);
}

void IrGenerator::emit_label(IR_Label label) {
  m_instructions.emplace_back(IROpCode::LABEL, label);
}

void IrGenerator::emit_func(IR_Label label) {
  m_instructions.emplace_back(IROpCode::FUNC, label);
}

void IrGenerator::emit_goto(IR_Label target) {
  m_instructions.emplace_back(IROpCode::GOTO, target);
}

void IrGenerator::emit_goto_true(IROperand cond, IR_Label target) {
  m_instructions.emplace_back(IROpCode::GOTO_T, cond, target);
}

void IrGenerator::emit_goto_false(IROperand cond, IR_Label target) {
  m_instructions.emplace_back(IROpCode::GOTO_F, cond, target);
}

void IrGenerator::emit_param(IROperand src) {
  m_instructions.emplace_back(IROpCode::PARAM, src);
}

void IrGenerator::emit_call(std::optional<IR_Register> dest,
                            IROperand func_target, IR_Immediate num_args) {
  m_instructions.emplace_back(IROpCode::CALL, dest, func_target, num_args);
}

void IrGenerator::emit_ret() {
  m_instructions.emplace_back(IROpCode::RET); // void return
}

void IrGenerator::emit_ret(IROperand retval) {
  m_instructions.emplace_back(IROpCode::RET, retval);
}

void IrGenerator::emit_read(IR_Register dest) {
  m_instructions.emplace_back(IROpCode::READ, dest);
}

void IrGenerator::emit_print(IROperand src) {
  m_instructions.emplace_back(IROpCode::PRINT, src);
}

void IrGenerator::emit_asm_block(const std::string& asm_code) {
  m_instructions.emplace_back(IROpCode::ASM_BLOCK, asm_code);
}

const std::vector<IRInstruction>& IrGenerator::get_instructions() const {
  return m_instructions;
}
