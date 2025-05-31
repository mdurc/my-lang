#ifndef CODEGEN_IR_IR_GENERATOR_H
#define CODEGEN_IR_IR_GENERATOR_H

#include <vector>

#include "ir_instruction.h"

class IrGenerator {
public:
  IrGenerator();

  IR_Register newRegister();

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

  const std::vector<IRInstruction>& getInstructions() const;

private:
  std::vector<IRInstruction> m_instructions;
  int m_next_reg_id = 0;
};

#endif // CODEGEN_IR_IR_GENERATOR_H
