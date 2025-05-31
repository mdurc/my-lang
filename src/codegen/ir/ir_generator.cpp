#include "ir_generator.h"

IrGenerator::IrGenerator() : m_next_reg_id(0) {}

IR_Register IrGenerator::newRegister() { return IR_Register(m_next_reg_id++); }

void IrGenerator::emitAdd(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::ADD, dest, src1, src2);
}

void IrGenerator::emitSub(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::SUB, dest, src1, src2);
}

void IrGenerator::emitMul(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::MUL, dest, src1, src2);
}

void IrGenerator::emitDiv(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::DIV, dest, src1, src2);
}

void IrGenerator::emitMov(IR_Register dest, IROperand src) {
  m_instructions.emplace_back(IROpCode::MOV, dest, src);
}

const std::vector<IRInstruction>& IrGenerator::getInstructions() const {
  return m_instructions;
}
