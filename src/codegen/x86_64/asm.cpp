#include "asm.h"

#include <algorithm>

X86_64CodeGenerator::X86_64CodeGenerator()
    : m_out(nullptr), m_next_available_reg_idx(0), m_current_stack_offset(0) {
  // Conventions:
  // - callee-saved: rsp, rbp, rbx, r12, r13, r14, r15
  // - caller-saved: rax, rcx, rdx, rsi, rdi, r8-11
  // - rax and r11 are not preserved during syscall
  // - Return value stored in rax, rax = 0 in the case of void return type
  // - Arguments: rdi, rsi, rdx, rcx, r8, r9, then on the stack

  m_physical_regs_pool = {"r10", "r11", "rbx", "r12", "r13", "r14", "r15"};
  m_callee_saved_regs = {"rsp", "rbp", "rbx", "r12", "r13", "r14", "r15"};
  m_caller_saved_regs = {"rax", "rcx", "rdx", "rsi", "r8", "r9", "r10", "r11"};
  m_arg_regs = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
}

std::string X86_64CodeGenerator::get_x86_reg(const IR_Register& ir_reg) {
  if (m_ir_reg_to_x86_reg.find(ir_reg.id) == m_ir_reg_to_x86_reg.end()) {
    if (m_next_available_reg_idx >= m_physical_regs_pool.size()) {
      throw std::runtime_error(
          "Ran out of physical registers. Spilling not implemented.");
    }
    std::string assigned_reg = m_physical_regs_pool[m_next_available_reg_idx++];
    m_ir_reg_to_x86_reg[ir_reg.id] = assigned_reg;

    if (std::find(m_callee_saved_regs.begin(), m_callee_saved_regs.end(),
                  assigned_reg) != m_callee_saved_regs.end() &&
        assigned_reg != "rbp") { // rbp is handled specially
      m_used_callee_saved_regs_in_current_func.insert(assigned_reg);
    }
  }
  return m_ir_reg_to_x86_reg[ir_reg.id];
}

std::string X86_64CodeGenerator::operand_to_string(const IROperand& operand) {
  if (std::holds_alternative<IR_Register>(operand)) {
    return get_x86_reg(std::get<IR_Register>(operand));
  } else if (std::holds_alternative<IR_Immediate>(operand)) {
    return std::to_string(std::get<IR_Immediate>(operand).val);
  } else if (std::holds_alternative<IR_Label>(operand)) {
    return ".L" + std::to_string(std::get<IR_Label>(operand).id);
  }
  // std::string is not included here as option because it is already a string
  throw std::runtime_error("Unknown IROperand type");
}

void X86_64CodeGenerator::emit(const std::string& instruction) {
  *m_out << "\t" << instruction << std::endl;
}

void X86_64CodeGenerator::emit_label(const std::string& label_name) {
  *m_out << label_name << ":" << std::endl;
}

void X86_64CodeGenerator::generate(
    const std::vector<IRInstruction>& instructions, std::ostream& out) {
  m_out = &out;
  m_ir_reg_to_x86_reg.clear();
  m_next_available_reg_idx = 0;
  m_current_stack_offset = 0;

  // preamble
  *m_out << "extern exit, string_length, print_string, print_char" << std::endl;
  *m_out << "extern print_newline, print_uint, print_int" << std::endl;
  *m_out << "extern read_char, read_word, parse_uint" << std::endl;
  *m_out << "extern parse_int, string_equals, string_copy" << std::endl;

  *m_out << std::endl;

  *m_out << "global _start" << std::endl;
  *m_out << "section .data" << std::endl;
  *m_out << "section .text" << std::endl;
  *m_out << "_start:" << std::endl;

  std::vector<IROperand> current_params;
  for (const IRInstruction& instr : instructions) {
    if (instr.opcode == IROpCode::PARAM) {
      current_params.push_back(instr.operands[0]);
      continue;
    }

    if (instr.opcode == IROpCode::CALL) {
      size_t sz = current_params.size();
      for (size_t i = 0; i < sz; ++i) {
        if (i < m_arg_regs.size()) {
          emit("mov " + m_arg_regs[i] + ", " +
               operand_to_string(current_params[i]));
        } else {
          // TODO: add stack parameters in reverse order
        }
      }
      // handle the call itself
      handle_instruction(instr);
      current_params.clear();
    } else {
      assert(current_params.empty() && "Params should be followed by a call");
      handle_instruction(instr);
    }
  }

  // epligoue
  emit("mov rdi, 0 ; exit code 0");
  emit("call exit"); // from x86_64_lib
}

void X86_64CodeGenerator::handle_instruction(const IRInstruction& instr) {
  switch (instr.opcode) {
    case IROpCode::MOV: handle_mov(instr); break;
    case IROpCode::ADD: handle_add(instr); break;
    case IROpCode::SUB: handle_sub(instr); break;
    case IROpCode::MUL: handle_mul(instr); break;
    case IROpCode::DIV: handle_div(instr); break;
    case IROpCode::MOD: handle_mod(instr); break;
    case IROpCode::NEG: handle_neg(instr); break;
    case IROpCode::CMP_EQ:
    case IROpCode::CMP_NE:
    case IROpCode::CMP_LT:
    case IROpCode::CMP_LE:
    case IROpCode::CMP_GT:
    case IROpCode::CMP_GE: handle_cmp(instr); break;
    case IROpCode::LABEL: handle_label(instr); break;
    case IROpCode::FUNC:
      m_used_callee_saved_regs_in_current_func.clear();
      m_ir_reg_to_x86_reg.clear();
      m_next_available_reg_idx = 0;
      handle_func(instr);
      break;
    case IROpCode::GOTO: handle_goto(instr); break;
    case IROpCode::GOTO_T: handle_goto_cond(instr, "jne"); break;
    case IROpCode::GOTO_F: handle_goto_cond(instr, "je"); break;
    case IROpCode::CALL: handle_call(instr); break;
    case IROpCode::RET: handle_ret(instr); break;
    case IROpCode::PRINT: handle_print(instr); break;
    case IROpCode::READ: handle_read(instr); break;
    case IROpCode::ASM_BLOCK: handle_asm_block(instr); break;
    // TODO: AND, OR, NOT
    default:
      throw std::runtime_error("Unsupported IR opcode for x86_64: " +
                               std::to_string((int)instr.opcode));
  }
}

void X86_64CodeGenerator::handle_mov(const IRInstruction& instr) {
  std::string dest_reg =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src_op = operand_to_string(instr.operands[0]);
  emit("mov " + dest_reg + ", " + src_op);
}

void X86_64CodeGenerator::handle_add(const IRInstruction& instr) {
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src1_str = operand_to_string(instr.operands[0]);
  std::string src2_str = operand_to_string(instr.operands[1]);

  emit("mov " + dest_reg_str + ", " + src1_str);
  emit("add " + dest_reg_str + ", " + src2_str);
}

void X86_64CodeGenerator::handle_sub(const IRInstruction& instr) {
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src1_str = operand_to_string(instr.operands[0]);
  std::string src2_str = operand_to_string(instr.operands[1]);

  emit("mov " + dest_reg_str + ", " + src1_str);
  emit("sub " + dest_reg_str + ", " + src2_str);
}

void X86_64CodeGenerator::handle_mul(const IRInstruction& instr) {
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src1_str = operand_to_string(instr.operands[0]);
  std::string src2_str = operand_to_string(instr.operands[1]);

  emit("mov " + dest_reg_str + ", " + src1_str);
  emit("imul " + dest_reg_str + ", " + src2_str);
}

void X86_64CodeGenerator::handle_div(const IRInstruction& instr) {
  // idiv r/m64: RDX:RAX / r/m64. Quotient in RAX, Remainder in RDX.
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src1_str = operand_to_string(instr.operands[0]); // Dividend
  std::string src2_str = operand_to_string(instr.operands[1]); // Divisor

  emit("; --- DIV start ---");
  emit("push rax");
  emit("push rdx");

  emit("mov rax, " + src1_str); // Load dividend into RAX
  emit("cqo");                  // Sign-extend RAX into RDX:RAX

  // RDX:RAX / src2. Quotient in RAX, Remainder in RDX.
  emit("idiv " + src2_str);

  emit("mov " + dest_reg_str + ", rax"); // Store quotient

  emit("pop rdx");
  emit("pop rax");
  emit("; --- DIV end ---");
}

void X86_64CodeGenerator::handle_mod(const IRInstruction& instr) {
  // idiv r/m64: RDX:RAX / r/m64. Quotient in RAX, Remainder in RDX.
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src1_str = operand_to_string(instr.operands[0]);
  std::string src2_str = operand_to_string(instr.operands[1]);

  emit("; --- MOD start ---");
  emit("push rax");
  emit("push rdx");

  emit("mov rax, " + src1_str);
  emit("cqo");
  emit("idiv " + src2_str);
  emit("mov " + dest_reg_str + ", rdx"); // Store remainder
  emit("pop rdx");
  emit("pop rax");
  emit("; --- MOD end ---");
}

void X86_64CodeGenerator::handle_neg(const IRInstruction& instr) {
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src_str = operand_to_string(instr.operands[0]);
  emit("mov " + dest_reg_str + ", " + src_str);
  emit("neg " + dest_reg_str);
}

void X86_64CodeGenerator::handle_cmp(const IRInstruction& instr) {
  // Result of CMP is boolean, stored in dest_reg
  // Operands are src1, src2
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src1_str = operand_to_string(instr.operands[0]);
  std::string src2_str = operand_to_string(instr.operands[1]);

  emit("push rax"); // Save rax as 'al' will be used

  emit("cmp " + src1_str + ", " + src2_str);
  std::string set_instr;
  std::string byte_reg = "al";
  switch (instr.opcode) {
    case IROpCode::CMP_EQ: set_instr = "sete"; break;
    case IROpCode::CMP_NE: set_instr = "setne"; break;
    case IROpCode::CMP_LT: set_instr = "setl"; break;
    case IROpCode::CMP_LE: set_instr = "setle"; break;
    case IROpCode::CMP_GT: set_instr = "setg"; break;
    case IROpCode::CMP_GE: set_instr = "setge"; break;
    default: throw std::runtime_error("Unhandled CMP type in handle_cmp");
  }
  emit(set_instr + " " + byte_reg);
  emit("movzx " + dest_reg_str + ", " + byte_reg); // Zero-extend al to dest_reg
  emit("pop rax");                                 // Restore rax
}

void X86_64CodeGenerator::handle_label(const IRInstruction& instr) {
  emit_label(operand_to_string(instr.result.value()));
}

void X86_64CodeGenerator::handle_func(const IRInstruction& instr) {
  emit_label(operand_to_string(instr.result.value()));
  emit("push rbp");
  emit("mov rbp, rsp");

  for (const std::string& reg_name : m_used_callee_saved_regs_in_current_func) {
    if (reg_name != "rbp") {
      emit("push " + reg_name);
    }
  }

  // TODO: Calculate actual stack space needed for locals and spills
  // m_current_stack_offset should be updated by variable declarations/spills
}

void X86_64CodeGenerator::handle_goto(const IRInstruction& instr) {
  emit("jmp " + operand_to_string(instr.operands[0]));
}

void X86_64CodeGenerator::handle_goto_cond(const IRInstruction& instr,
                                           const std::string& jmp_mnemonic) {
  // instr.operands[0] is the condition register
  // instr.operands[1] is the target label
  std::string cond_reg_str = operand_to_string(instr.operands[0]);
  std::string target_label_str = operand_to_string(instr.operands[1]);

  // see if it is zero or not
  emit("test " + cond_reg_str + ", " + cond_reg_str);
  emit(jmp_mnemonic + " " + target_label_str);
}

void X86_64CodeGenerator::handle_call(const IRInstruction& instr) {
  std::string func_target_str = operand_to_string(instr.operands[0]);
  emit("call " + func_target_str);

  // If the call has a result, it's in RAX. Move it to the IR result register.
  if (instr.result.has_value() &&
      std::holds_alternative<IR_Register>(instr.result.value())) {
    std::string dest_ir_reg_str =
        get_x86_reg(std::get<IR_Register>(instr.result.value()));
    emit("mov " + dest_ir_reg_str + ", rax");
  }
}

void X86_64CodeGenerator::handle_ret(const IRInstruction& instr) {
  if (!instr.operands.empty()) {
    // Non-void return
    std::string retval_str = operand_to_string(instr.operands[0]);
    emit("mov rax, " + retval_str); // Return value in RAX
  } else {
    emit("xor rax, rax"); // store zero in rax
  }

  // epilogue
  for (auto it = m_used_callee_saved_regs_in_current_func.rbegin();
       it != m_used_callee_saved_regs_in_current_func.rend(); ++it) {
    if (*it != "rbp") {
      emit("pop " + *it);
    }
  }
  emit("mov rsp, rbp");
  emit("pop rbp");
  emit("ret");
}

void X86_64CodeGenerator::handle_print(const IRInstruction& instr) {
  // uses runtime x86_64_lib.asm
  // print_int expects the integer in rdi.
  // TODO: once we implement string literals, we can use print_string
  std::string src_op_str = operand_to_string(instr.operands[0]);
  emit("mov rdi, " + src_op_str);
  emit("call print_int");
}

void X86_64CodeGenerator::handle_read(const IRInstruction& instr) {
  // uses runtime x86_64_lib.asm
  // TODO: extend to strings too
  assert(instr.result.has_value() &&
         std::holds_alternative<IR_Register>(instr.result.value()));

  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));

  // Allocate a temporary buffer on the stack for read_word
  // The buffer size in x86_64_lib.asm for parse_uint/int is 32 bytes.
  emit("sub rsp, 32");
  emit("mov rdi, rsp");
  emit("mov rsi, 32");
  emit("call read_word");

  emit("mov rdi, rsp");
  emit("call parse_int");

  emit("mov " + dest_reg_str + ", rax"); // Move parsed int to destination
  emit("add rsp, 32");                   // cleanup stack
}
void X86_64CodeGenerator::handle_asm_block(const IRInstruction& instr) {
  assert(!instr.operands.empty() &&
         std::holds_alternative<std::string>(instr.operands[0]) &&
         "std::string operand must exist for ASM_BLOCK in IR");

  const std::string& asm_code = std::get<std::string>(instr.operands[0]);
  *m_out << asm_code << std::endl;
}
