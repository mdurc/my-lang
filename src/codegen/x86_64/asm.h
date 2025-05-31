#ifndef CODEGEN_X86_64_ASM_H
#define CODEGEN_X86_64_ASM_H

#include <iostream>
#include <set>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

#include "../ir/ir_instruction.h"

class X86_64CodeGenerator {
public:
  X86_64CodeGenerator();

  void generate(const std::vector<IRInstruction>& instructions,
                std::ostream& out);

private:
  std::ostream* m_out;
  std::unordered_map<int, std::string> m_ir_reg_to_x86_reg;
  size_t m_next_available_reg_idx;
  int m_current_stack_offset;

  std::vector<std::string> m_physical_regs_pool;
  std::vector<std::string> m_callee_saved_regs;
  std::vector<std::string> m_caller_saved_regs;
  std::vector<std::string> m_arg_regs;
  std::set<std::string> m_used_callee_saved_regs_in_current_func;

  std::string get_x86_reg(const IR_Register& ir_reg);
  std::string operand_to_string(const IROperand& operand);
  void emit(const std::string& instruction);
  void emit_label(const std::string& label_name);

  void handle_instruction(const IRInstruction& instr);

  void handle_mov(const IRInstruction& instr);
  void handle_add(const IRInstruction& instr);
  void handle_sub(const IRInstruction& instr);
  void handle_mul(const IRInstruction& instr);
  void handle_div(const IRInstruction& instr);
  void handle_mod(const IRInstruction& instr);
  void handle_neg(const IRInstruction& instr);
  void handle_cmp(const IRInstruction& instr); // For all CMP_XX
  void handle_label(const IRInstruction& instr);
  void handle_func(const IRInstruction& instr);
  void handle_goto(const IRInstruction& instr);
  void handle_goto_cond(const IRInstruction& instr,
                        const std::string& jmp_mnemonic); // GOTO_T, GOTO_F
  void handle_param(const IRInstruction& instr, int param_idx);
  void handle_call(const IRInstruction& instr);
  void handle_ret(const IRInstruction& instr);
  void handle_print(const IRInstruction& instr);
  void handle_read(const IRInstruction& instr);
};

#endif // CODEGEN_X86_64_ASM_H
