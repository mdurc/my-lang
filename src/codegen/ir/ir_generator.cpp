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

void IrGenerator::emitMod(IR_Register dest, IROperand src1, IROperand src2) {
  m_instructions.emplace_back(IROpCode::MOD, dest, src1, src2);
}

void IrGenerator::emitLabel(IR_Label label) {
  m_instructions.emplace_back(IROpCode::LABEL, label);
}

void IrGenerator::emitFunc(IR_Label label) {
  m_instructions.emplace_back(IROpCode::FUNC, label);
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

void IrGenerator::emitParam(IROperand src) {
  m_instructions.emplace_back(IROpCode::PARAM, src);
}

void IrGenerator::emitCall(std::optional<IR_Register> dest,
                           IROperand func_target, IR_Immediate num_args) {
  m_instructions.emplace_back(IROpCode::CALL, dest, func_target, num_args);
}

void IrGenerator::emitRet() {
  m_instructions.emplace_back(IROpCode::RET); // void return
}

void IrGenerator::emitRet(IROperand retval) {
  m_instructions.emplace_back(IROpCode::RET, retval);
}

void IrGenerator::emitRead(IR_Register dest) {
  m_instructions.emplace_back(IROpCode::READ, dest);
}

void IrGenerator::emitPrint(IROperand src) {
  m_instructions.emplace_back(IROpCode::PRINT, src);
}

void IrGenerator::emitAsmBlock(const std::string& asm_code) {
  m_instructions.emplace_back(IROpCode::ASM_BLOCK, asm_code);
}

const std::vector<IRInstruction>& IrGenerator::getInstructions() const {
  return m_instructions;
}
