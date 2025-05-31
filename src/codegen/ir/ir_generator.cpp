#include "ir_generator.h"

IrGenerator::IrGenerator() : m_next_reg_id(0), m_next_label_id(0) {}

IR_Register IrGenerator::newRegister() { return IR_Register(m_next_reg_id++); }
IR_Label IrGenerator::newLabel() { return IR_Label(m_next_label_id++); }

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

void IrGenerator::emitNeg(IR_Register dest, IROperand src) {
  m_instructions.emplace_back(IROpCode::NEG, dest, src);
}

void IrGenerator::emitLogicalNot(IR_Register dest, IROperand src) {
  m_instructions.emplace_back(IROpCode::NOT, dest, src);
}

void IrGenerator::emitCmpEq(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_EQ, dest, src1, src2);
}

void IrGenerator::emitCmpNe(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_NE, dest, src1, src2);
}

void IrGenerator::emitCmpLt(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_LT, dest, src1, src2);
}

void IrGenerator::emitCmpLe(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_LE, dest, src1, src2);
}

void IrGenerator::emitCmpGt(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_GT, dest, src1, src2);
}

void IrGenerator::emitCmpGe(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::CMP_GE, dest, src1, src2);
}
void IrGenerator::emitLogicalAnd(IR_Register dest, IROperand src1,
                                 IROperand src2) {
  m_instructions.emplace_back(IROpCode::AND, dest, src1, src2);
}

void IrGenerator::emitLogicalOr(IR_Register dest, IROperand src1,
                                IROperand src2) {
  m_instructions.emplace_back(IROpCode::OR, dest, src1, src2);
}

void IrGenerator::emitLabel(IR_Label label) {
  m_instructions.emplace_back(IROpCode::LABEL, label);
}

void IrGenerator::emitGoto(IR_Label target) {
  m_instructions.emplace_back(IROpCode::GOTO, target);
}

void IrGenerator::emitGotoTrue(IROperand cond, IR_Label target) {
  m_instructions.emplace_back(IROpCode::GOTO_T, cond, target);
}

void IrGenerator::emitGotoFalse(IROperand cond, IR_Label target) {
  m_instructions.emplace_back(IROpCode::GOTO_F, cond, target);
}

const std::vector<IRInstruction>& IrGenerator::getInstructions() const {
  return m_instructions;
}
