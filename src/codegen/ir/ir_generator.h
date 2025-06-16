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
  void emit_end_func(IROperand return_val, uint64_t return_size);
  void emit_end_func();
  void emit_exit(int exit_code);

  // Assignment and Data
  void emit_assign(IROperand dst, IROperand src, uint64_t size);
  // dst = *addr
  void emit_load(IR_Register dst, IROperand addr_src, uint64_t size);
  // *addr = src
  void emit_store(IROperand address_dest, IROperand src, uint64_t size);

  // Arithmetic / Logical
  void emit_add(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);
  void emit_sub(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);
  void emit_mul(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);
  void emit_div(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);
  void emit_mod(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);
  void emit_neg(IR_Register dst, IROperand src, uint64_t size);
  void emit_log_not(IR_Register dst, IROperand s, uint64_t size);
  void emit_log_and(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);
  void emit_log_or(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);

  // Comparison
  void emit_cmp_eq(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);
  void emit_cmp_ne(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);
  void emit_cmp_lt(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);
  void emit_cmp_le(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);
  void emit_cmp_gt(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);
  void emit_cmp_ge(IR_Register dst, IROperand s1, IROperand s2, uint64_t size);
  void emit_cmp_str_eq(IR_Register dst, IROperand s1, IROperand s2,
                       uint64_t size);

  // Control Flow
  void emit_label(IR_Label label);
  void emit_goto(IR_Label target);
  void emit_if_z(IROperand cond, IR_Label target,
                 uint64_t cond_operands_size); // IfZero (false)

  // Procedure Calls
  void emit_push_arg(IROperand src, uint64_t arg_size);
  void emit_lcall(std::optional<IR_Register> dst, IROperand func_target,
                  uint64_t return_size);

  // Raw Assembly
  void emit_asm_block(const std::string& asm_code);

  // Pointer & Memory
  void emit_addr_of(IR_Register dst, IROperand src_lval);
  void emit_alloc(IR_Register dst_ptr, uint64_t type_size,
                  std::optional<IROperand> initializer,
                  uint64_t initializer_type_size);
  void emit_alloc_array(IR_Register dst_ptr, uint64_t size_el, IROperand num_el,
                        uint64_t initializer_type_size);
  void emit_free(IROperand ptr);

  const std::vector<IRInstruction>& get_instructions() const;

private:
  std::vector<IRInstruction> m_instructions;
  int m_next_reg_id;
  int m_next_label_id;
};

#endif // CODEGEN_IR_IR_GENERATOR_H
