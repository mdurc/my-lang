#ifndef CODEGEN_IR_IR_GENERATOR_H
#define CODEGEN_IR_IR_GENERATOR_H

#include <vector>

#include "ir_instruction.h"

class IrGenerator {
public:
  IrGenerator();

  IR_Register new_temp_reg();
  IR_Label new_label();
  IR_Label new_func_label(const std::string& func_name);

  // Function demarcation
  void emit_begin_func(IR_Label func_label);
  void emit_end_func();
  void emit_exit();

  // Assignment and Data
  void emit_assign(IROperand dest, IROperand src);
  void emit_load(IR_Register dest, IROperand addr_src);   // dest = *address_src
  void emit_store(IROperand address_dest, IROperand src); // *address_dest = src

  // Arithmetic / Logical
  void emit_add(IR_Register dest, IROperand src1, IROperand src2);
  void emit_sub(IR_Register dest, IROperand src1, IROperand src2);
  void emit_mul(IR_Register dest, IROperand src1, IROperand src2);
  void emit_div(IR_Register dest, IROperand src1, IROperand src2);
  void emit_mod(IR_Register dest, IROperand src1, IROperand src2);
  void emit_neg(IR_Register dest, IROperand src);
  void emit_logical_not(IR_Register dest, IROperand src);
  void emit_logical_and(IR_Register dest, IROperand src1, IROperand src2);
  void emit_logical_or(IR_Register dest, IROperand src1, IROperand src2);

  // Comparison
  void emit_cmp_eq(IR_Register dest, IROperand src1, IROperand src2);
  void emit_cmp_ne(IR_Register dest, IROperand src1, IROperand src2);
  void emit_cmp_lt(IR_Register dest, IROperand src1, IROperand src2);
  void emit_cmp_le(IR_Register dest, IROperand src1, IROperand src2);
  void emit_cmp_gt(IR_Register dest, IROperand src1, IROperand src2);
  void emit_cmp_ge(IR_Register dest, IROperand src1, IROperand src2);

  // Control Flow
  void emit_label(IR_Label label);
  void emit_goto(IR_Label target);
  void emit_if_z(IROperand cond, IR_Label target); // IfZero (false)

  // Procedure Calls
  void emit_push_arg(IROperand src);
  void emit_pop_args(IR_Immediate num_bytes);
  void emit_lcall(std::optional<IR_Register> dest, IR_Label func_target);
  void emit_ret();
  void emit_ret(IROperand retval);

  // Raw Assembly
  void emit_asm_block(const std::string& asm_code);

  const std::vector<IRInstruction>& get_instructions() const;

private:
  std::vector<IRInstruction> m_instructions;
  int m_next_reg_id;
  int m_next_label_id;
};

#endif // CODEGEN_IR_IR_GENERATOR_H
