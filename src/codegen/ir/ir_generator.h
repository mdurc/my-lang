#ifndef CODEGEN_IR_IR_GENERATOR_H
#define CODEGEN_IR_IR_GENERATOR_H

#include <vector>

#include "ir_instruction.h"

class IrGenerator {
public:
  IrGenerator();

  IR_Register newRegister();
  IR_Label newLabel();

  // Emits an ADD instruction: dest = src1 + src2
  void emitAdd(IR_Register dest, IROperand src1, IROperand src2);
  // Emits a SUB instruction: dest = src1 - src2
  void emitSub(IR_Register dest, IROperand src1, IROperand src2);
  // Emits a MUL instruction: dest = src1 * src2
  void emitMul(IR_Register dest, IROperand src1, IROperand src2);
  // Emits a DIV instruction: dest = src1 / src2
  void emitDiv(IR_Register dest, IROperand src1, IROperand src2);
  // Emits a MOV instruction: dest = src (src can be imm or reg)
  void emitMov(IR_Register dest, IROperand src);
  // Emits a NEG instruction: dest = -src
  void emitNeg(IR_Register dest, IROperand src);
  // Emits a NOT instruction: dest_bool = !src_bool
  void emitLogicalNot(IR_Register dest, IROperand src);
  // Comparison: dest_bool = src1 op src2
  void emitCmpEq(IR_Register dest, IROperand src1, IROperand src2);
  void emitCmpNe(IR_Register dest, IROperand src1, IROperand src2);
  void emitCmpLt(IR_Register dest, IROperand src1, IROperand src2);
  void emitCmpLe(IR_Register dest, IROperand src1, IROperand src2);
  void emitCmpGt(IR_Register dest, IROperand src1, IROperand src2);
  void emitCmpGe(IR_Register dest, IROperand src1, IROperand src2);
  // Logical: dest_bool = src1 op src2
  void emitLogicalAnd(IR_Register dest, IROperand src1, IROperand src2);
  void emitLogicalOr(IR_Register dest, IROperand src1, IROperand src2);
  // Control Flow
  void emitLabel(IR_Label label);
  void emitGoto(IR_Label target);
  void emitGotoTrue(IROperand cond, IR_Label target);
  void emitGotoFalse(IROperand cond, IR_Label target);

  const std::vector<IRInstruction>& getInstructions() const;

private:
  std::vector<IRInstruction> m_instructions;
  int m_next_reg_id;
  int m_next_label_id;
};

#endif // CODEGEN_IR_IR_GENERATOR_H
