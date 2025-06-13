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

  bool m_handling_top_level;
  size_t m_global_var_alloc;
  std::unordered_map<std::string, std::string> m_var_locations; // in stack
  std::unordered_map<std::string, std::string> m_glob_var_locations;

  bool m_is_buffering_function;
  std::vector<std::string> m_current_func_asm_buffer;
  size_t m_current_func_alloc_placeholder_idx;
  size_t m_current_func_stack_offset;
  std::stack<size_t> m_allocated_arg_bytes;

  size_t m_current_call_arg_count;

  std::vector<std::string> m_string_literals_data;
  std::unordered_map<std::string, std::string> m_string_literal_to_label;
  size_t m_string_count;

  // register allocation
  // {x86_reg_str, <IR_reg_id, reg_size>}
  std::unordered_map<std::string, std::pair<int, uint64_t>> m_x86_reg_to_ir_reg;
  std::unordered_map<int, std::string> m_ir_reg_to_x86_reg;
  std::unordered_map<int, std::pair<std::string, uint64_t>>
      m_spilled_ir_reg_locations;
  size_t m_reg_count;
  std::vector<std::string> m_temp_regs;
  std::vector<std::string> m_callee_saved_regs;
  std::vector<std::string> m_caller_saved_regs;
  std::vector<std::string> m_arg_regs;
  std::set<std::string> m_used_callee_saved;

  void clear_func_data();

  std::string get_size_prefix(uint64_t size);
  std::string get_sized_register_name(const std::string& reg64_name,
                                      uint64_t size);
  std::string operand_to_string(const IROperand& operand);
  std::string get_sized_component(const IROperand& operand, uint64_t size);

  void spill_register(const std::string& reg, uint64_t size, int new_id);
  std::string get_temp_x86_reg(uint64_t size);
  std::string get_x86_reg(const IR_Register& ir_reg);

  void emit_one_operand_memory_operation(const IROperand& s1,
                                         const IROperand& s2,
                                         const std::string& operation,
                                         uint64_t size);
  void emit_one_operand_memory_operation(const std::string& s1_str,
                                         const std::string& s2_str,
                                         const std::string& operation,
                                         uint64_t size);
  void emit(const std::string& instruction);
  void emit_label(const std::string& label_name);

  void handle_instruction(const IRInstruction& instr);

  void handle_begin_func(const IRInstruction& instr);
  void handle_end_preamble();
  void handle_end_func(const IRInstruction* instr, bool exit);
  void handle_exit(int code);

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
  void handle_asm_block(const IRInstruction& instr);
};

#endif // CODEGEN_X86_64_ASM_H
