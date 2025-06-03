#ifndef CODEGEN_X86_64_ASM_H
#define CODEGEN_X86_64_ASM_H

#include <iostream>
#include <list>
#include <set>
#include <stack>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

#include "../ir/ir_instruction.h"

class X86_64CodeGenerator {
public:
  X86_64CodeGenerator();

  void generate(const std::vector<IRInstruction>& instructions,
                bool is_main_defined, std::ostream& out);

private:
  std::ostream* m_out;

  std::vector<std::string> m_current_function_asm_buffer;
  size_t m_stack_alloc_placeholder_idx;
  bool m_is_buffering_function;

  std::unordered_map<int, std::string> m_ir_reg_to_x86_reg;
  int m_current_stack_offset;

  int m_current_arg_count;
  std::stack<int> m_allocated_arg_bytes;

  std::unordered_map<std::string, std::string> m_var_locations; // in stack
  std::vector<std::string> m_string_literals_data;
  std::unordered_map<std::string, std::string> m_string_literal_to_label;

  // Register allocation and spilling
  std::unordered_map<std::string, int> m_x86_reg_to_ir_reg;
  std::unordered_map<int, int> m_spilled_reg_locations; // ir reg id, rbp offset
  std::list<std::string> m_physical_reg_lru;

  std::vector<std::string> m_temp_regs;
  std::vector<std::string> m_callee_saved_regs;
  std::vector<std::string> m_caller_saved_regs;
  std::vector<std::string> m_arg_regs;
  std::set<std::string> m_used_callee_saved_regs_in_current_func;

  std::string get_temp_x86_reg();
  std::string get_x86_reg(const IR_Register& ir_reg);
  std::string operand_to_string(const IROperand& operand);
  std::string internal_acquire_phys_reg();
  void update_lru(const std::string& phys_reg_name);

  // this is a helper that ensures that the mov between these two operands
  // will be valid in that they won't both be memory content accesses, or
  // both immediates, or anything that doesn't indicate size of ops to assembler
  void emit_move(IROperand dst, IROperand src, bool is_load, bool is_store);

  void emit(const std::string& instruction);
  void emit_label(const std::string& label_name);

  void handle_instruction(const IRInstruction& instr);

  void handle_begin_func(const IRInstruction& instr);
  void handle_end_func(const IRInstruction& instr);
  void handle_exit(const IRInstruction& instr);

  void handle_assign(const IRInstruction& instr);
  void handle_load(const IRInstruction& instr);
  void handle_store(const IRInstruction& instr);

  void handle_add(const IRInstruction& instr);
  void handle_sub(const IRInstruction& instr);
  void handle_mul(const IRInstruction& instr);
  void handle_div(const IRInstruction& instr);
  void handle_mod(const IRInstruction& instr);
  void handle_neg(const IRInstruction& instr);
  void handle_logical_not(const IRInstruction& instr);
  void handle_logical_and_or(const IRInstruction& instr,
                             const std::string& op_mnemonic);

  void handle_cmp(const IRInstruction& instr); // For all CMP_XX

  void handle_label(const IRInstruction& instr);
  void handle_goto(const IRInstruction& instr);
  void handle_if_z(const IRInstruction& instr);

  void handle_push_arg(const IRInstruction& instr);
  void handle_pop_args(const IRInstruction& instr);
  void handle_lcall(const IRInstruction& instr);
  void handle_ret(const IRInstruction& instr);
  void handle_asm_block(const IRInstruction& instr);
};

#endif // CODEGEN_X86_64_ASM_H
