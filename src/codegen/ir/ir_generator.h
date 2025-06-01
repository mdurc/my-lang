#ifndef CODEGEN_IR_IR_GENERATOR_H
#define CODEGEN_IR_IR_GENERATOR_H

#include <vector>

#include "ir_instruction.h"

class IrGenerator {
public:
  IrGenerator();

  IR_Register new_register();
  IR_Label new_label();

  // Emits an ADD instruction: dest = src1 + src2
  void emit_add(IR_Register dest, IROperand src1, IROperand src2);
  // Emits a SUB instruction: dest = src1 - src2
  void emit_sub(IR_Register dest, IROperand src1, IROperand src2);
  // Emits a MUL instruction: dest = src1 * src2
  void emit_mul(IR_Register dest, IROperand src1, IROperand src2);
  // Emits a DIV instruction: dest = src1 / src2
  void emit_div(IR_Register dest, IROperand src1, IROperand src2);
  // Emits a MOV instruction: dest = src (src can be imm or reg)
  void emit_mov(IR_Register dest, IROperand src);
  // Emits a NEG instruction: dest = -src
  void emit_neg(IR_Register dest, IROperand src);
  // Emits a NOT instruction: dest_bool = !src_bool
  void emit_logical_not(IR_Register dest, IROperand src);
  // Emits a MOD instruction: dest = src1 % src2
  void emit_mod(IR_Register dest, IROperand src1, IROperand src2);
  // Comparison: dest_bool = src1 op src2
  void emit_cmp_eq(IR_Register dest, IROperand src1, IROperand src2);
  void emit_cmp_ne(IR_Register dest, IROperand src1, IROperand src2);
  void emit_cmp_lt(IR_Register dest, IROperand src1, IROperand src2);
  void emit_cmp_le(IR_Register dest, IROperand src1, IROperand src2);
  void emit_cmp_gt(IR_Register dest, IROperand src1, IROperand src2);
  void emit_cmp_ge(IR_Register dest, IROperand src1, IROperand src2);
  // Logical: dest_bool = src1 op src2
  void emit_logical_and(IR_Register dest, IROperand src1, IROperand src2);
  void emit_logical_or(IR_Register dest, IROperand src1, IROperand src2);
  // Control Flow
  void emit_label(IR_Label label);
  void emit_func(IR_Label label);
  void emit_goto(IR_Label target);
  void emit_goto_true(IROperand cond, IR_Label target);
  void emit_goto_false(IROperand cond, IR_Label target);
  // Procedure calls
  void emit_param(IROperand src);
  void emit_call(std::optional<IR_Register> dest, IROperand func_target,
                 IR_Immediate num_args);
  void emit_ret();
  void emit_ret(IROperand retval);
  // I/O
  void emit_read(IR_Register dest);
  void emit_print(IROperand src);
  // Raw Assembly
  void emit_asm_block(const std::string& asm_code);

  const std::vector<IRInstruction>& get_instructions() const;

private:
  std::vector<IRInstruction> m_instructions;
  int m_next_reg_id;
  int m_next_label_id;
};

#endif // CODEGEN_IR_IR_GENERATOR_H
