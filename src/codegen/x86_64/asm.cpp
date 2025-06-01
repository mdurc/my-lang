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
  } else if (std::holds_alternative<IR_Variable>(operand)) {
    const std::string& var_name = std::get<IR_Variable>(operand).name;
    assert(m_var_locations.count(var_name));
    return m_var_locations.at(var_name);
  } else if (std::holds_alternative<IR_Immediate>(operand)) {
    return std::to_string(std::get<IR_Immediate>(operand).val);
  } else if (std::holds_alternative<IR_Label>(operand)) {
    return std::get<IR_Label>(operand).name;
  } else if (std::holds_alternative<std::string>(operand)) { // String literal
    const std::string& str_val = std::get<std::string>(operand);
    assert(m_string_literal_to_label.count(str_val));
    return m_string_literal_to_label.at(str_val);
  }
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
  m_var_locations.clear();
  m_string_literals_data.clear();
  m_string_literal_to_label.clear();

  // Gather all string literals found
  int string_lit_idx = 0;
  for (const IRInstruction& instr : instructions) {
    auto process_operand_for_string = [&](const IROperand& op) {
      if (std::holds_alternative<std::string>(op)) {
        const std::string& str_val = std::get<std::string>(op);
        if (m_string_literal_to_label.find(str_val) ==
            m_string_literal_to_label.end()) {
          std::string label = ".LC" + std::to_string(string_lit_idx++);
          m_string_literal_to_label[str_val] = label;
          m_string_literals_data.push_back(str_val);
        }
      }
    };
    if (instr.result.has_value()) {
      process_operand_for_string(instr.result.value());
    }
    for (const IROperand& op : instr.operands) {
      process_operand_for_string(op);
    }
  }

  // preamble
  *m_out << "extern exit, string_length, print_string, print_char" << std::endl;
  *m_out << "extern print_newline, print_uint, print_int" << std::endl;
  *m_out << "extern read_char, read_word, parse_uint" << std::endl;
  *m_out << "extern parse_int, string_equals, string_copy" << std::endl;

  *m_out << std::endl;

  *m_out << "section .data" << std::endl;

  for (const std::string& str : m_string_literals_data) {
    std::string label_for_str;
    // Find label for this string_val (could be more efficient with reverse map)
    for (const auto& p : m_string_literal_to_label) {
      if (p.first == str) {
        label_for_str = p.second;
        break;
      }
    }
    *m_out << label_for_str << ":" << std::endl;
    *m_out << "\tdb \"";
    for (char c : str) {
      if (c == '"')
        *m_out << "\", `\"`, \""; // NASM escape for quote
      else if (c == '\n')
        *m_out << "\", 10, \""; // Newline
      else if (c == '\t')
        *m_out << "\", 9, \""; // Tab
      else if (c == 0)
        *m_out << "\", 0, \""; // Explicit null
      else if (c < 32 || c > 126) {
        *m_out << "\", " << static_cast<int>(static_cast<unsigned char>(c))
               << ", \"";
      } else {
        *m_out << c;
      }
    }
    *m_out << "\", 0" << std::endl;
  }

  *m_out << std::endl;
  *m_out << "global _start" << std::endl;

  *m_out << "section .text" << std::endl;

  *m_out << "_start:" << std::endl;
  *m_out << "\tcall main" << std::endl;
  // use main's return value as exit code
  *m_out << "\tmov rdi, rax" << std::endl;
  *m_out << "\tcall exit" << std::endl;

  std::vector<IROperand> current_params;
  for (const IRInstruction& instr : instructions) {
    handle_instruction(instr);
  }
}

void X86_64CodeGenerator::handle_instruction(const IRInstruction& instr) {
  switch (instr.opcode) {
    case IROpCode::BEGIN_FUNC: handle_begin_func(instr); break;
    case IROpCode::END_FUNC: handle_end_func(instr); break;
    case IROpCode::EXIT: handle_exit(instr); break;
    case IROpCode::ASSIGN: handle_assign(instr); break;
    case IROpCode::LOAD: handle_load(instr); break;
    case IROpCode::STORE: handle_store(instr); break;
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
    case IROpCode::NOT: handle_logical_not(instr); break;
    case IROpCode::AND: handle_logical_and_or(instr, "and"); break;
    case IROpCode::OR: handle_logical_and_or(instr, "or"); break;
    case IROpCode::LABEL: handle_label(instr); break;
    case IROpCode::GOTO: handle_goto(instr); break;
    case IROpCode::IF_Z: handle_if_z(instr); break;
    case IROpCode::PUSH_PARAM: handle_push_param(instr); break;
    case IROpCode::POP_PARAMS: handle_pop_params(instr); break;
    case IROpCode::LCALL: handle_lcall(instr); break;
    case IROpCode::RETURN: handle_ret(instr); break;
    case IROpCode::ASM_BLOCK: handle_asm_block(instr); break;
    default:
      throw std::runtime_error("Unsupported IR opcode for x86_64: " +
                               std::to_string((int)instr.opcode));
  }
}

void X86_64CodeGenerator::handle_begin_func(const IRInstruction& instr) {
  // instr.result has IR_Label func_label
  // instr.operands[0] has IR_Immediate stack_size
  m_used_callee_saved_regs_in_current_func.clear();
  m_ir_reg_to_x86_reg.clear();
  m_next_available_reg_idx = 0;
  m_var_locations.clear();
  m_current_stack_offset = 0;

  emit_label(operand_to_string(instr.result.value()));
  emit("push rbp");
  emit("mov rbp, rsp");

  uint64_t stack_size = std::get<IR_Immediate>(instr.operands[0]).val;
  if (stack_size > 0) {
    emit("sub rsp, " + std::to_string(stack_size));
  }
  // TODO: Handle saving callee-saved registers that will be used by this
  // function. This requires knowing which callee-saved regs are used by
  // IR_Registers.

  // TODO: Parameter handling
  // First 6 integer/pointer args in regs, the rest on stack
}

void X86_64CodeGenerator::handle_end_func(const IRInstruction&) {
  // TODO: Restore callee-saved registers (in reverse order of push)
  emit("mov rsp, rbp");
  emit("pop rbp");
  emit("ret");
}

void X86_64CodeGenerator::handle_exit(const IRInstruction&) {
  emit("mov rdi, 0");
  emit("call exit");
}

void X86_64CodeGenerator::handle_assign(const IRInstruction& instr) {
  std::string dest_op_str;
  bool dest_is_mem = false;

  if (std::holds_alternative<IR_Register>(instr.result.value())) {
    dest_op_str = get_x86_reg(std::get<IR_Register>(instr.result.value()));
  } else if (std::holds_alternative<IR_Variable>(instr.result.value())) {
    const IR_Variable& var = std::get<IR_Variable>(instr.result.value());
    if (!m_var_locations.count(var.name)) {
      m_current_stack_offset += 8;
      m_var_locations[var.name] =
          "[rbp - " + std::to_string(m_current_stack_offset) + "]";
    }
    assert(m_var_locations.count(var.name));
    dest_op_str = m_var_locations.at(var.name);
    dest_is_mem = true;
  } else {
    throw std::runtime_error(
        "Invalid destination type for ASSIGN IR instruction");
  }

  std::string src_op = operand_to_string(instr.operands[0]);

  if (dest_is_mem && !std::holds_alternative<IR_Immediate>(instr.operands[0]) &&
      !std::holds_alternative<std::string>(instr.operands[0])) {
    // If src_op is also memory (e.g., "[rbp-X]"), load src to temp reg first.
    if (src_op.front() == '[' && src_op.back() == ']') {
      std::string temp = get_x86_reg(IR_Register(m_next_available_reg_idx++));
      emit("mov " + temp + ", " + src_op);
      src_op = temp;
    }
    emit("mov " + dest_op_str + ", " + src_op); // Now mov [mem_dest], reg
  } else if (std::holds_alternative<std::string>(instr.operands[0])) {
    // string literal assignment
    // dest_op_str can be reg or [mem]
    // src_op will be a label like .LC0
    emit("lea " + dest_op_str + ", [" + src_op + "]");
  } else {
    emit("mov " + dest_op_str + ", " + src_op);
  }
}

void X86_64CodeGenerator::handle_load(const IRInstruction& instr) {
  // result: dest_temp (IR_Register), operands[0]: address_operand
  std::string dest_reg =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string addr_src_str = operand_to_string(instr.operands[0]);

  if (addr_src_str.find('[') == std::string::npos &&
      !isdigit(addr_src_str[0])) {
    emit("mov " + dest_reg + ", [" + addr_src_str + "]");
  } else {
    std::string temp = dest_reg;
    if (addr_src_str.front() == '[') {
      temp = get_x86_reg(IR_Register(m_next_available_reg_idx++));
      emit("mov " + temp + ", " + addr_src_str);
      emit("mov " + dest_reg + ", [" + temp + "]");
    } else {
      emit("mov " + dest_reg + ", [" + addr_src_str + "]");
    }
  }
}

void X86_64CodeGenerator::handle_store(const IRInstruction& instr) {
  // operands[0]: address_dest_operand, operands[1]: src_val_operand
  std::string addr_dest_op_str = operand_to_string(instr.operands[0]);
  std::string src_val_op_str = operand_to_string(instr.operands[1]);
  std::string final_addr_str;

  if (addr_dest_op_str.front() == '[') {
    std::string temp = get_x86_reg(IR_Register(m_next_available_reg_idx++));
    emit("mov " + temp + ", " + addr_dest_op_str);
    final_addr_str = "[" + temp + "]";
  } else {
    final_addr_str = "[" + addr_dest_op_str + "]";
  }

  // If src_val_op_str is memory, load to temp reg first
  if (src_val_op_str.front() == '[' && src_val_op_str.back() == ']') {
    std::string temp = get_x86_reg(IR_Register(m_next_available_reg_idx++));
    emit("mov " + temp + ", " + src_val_op_str);
    src_val_op_str = temp;
  }
  emit("mov " + final_addr_str + ", " + src_val_op_str);
}

void X86_64CodeGenerator::handle_add(const IRInstruction& instr) {
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src1_str = operand_to_string(instr.operands[0]);
  std::string src2_str = operand_to_string(instr.operands[1]);

  if (dest_reg_str != src1_str || src1_str.front() == '[') {
    emit("mov " + dest_reg_str + ", " + src1_str);
  }
  emit("add " + dest_reg_str + ", " + src2_str);
}

void X86_64CodeGenerator::handle_sub(const IRInstruction& instr) {
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src1_str = operand_to_string(instr.operands[0]);
  std::string src2_str = operand_to_string(instr.operands[1]);

  if (dest_reg_str != src1_str || src1_str.front() == '[') {
    emit("mov " + dest_reg_str + ", " + src1_str);
  }
  emit("sub " + dest_reg_str + ", " + src2_str);
}

void X86_64CodeGenerator::handle_mul(const IRInstruction& instr) {
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src1_str = operand_to_string(instr.operands[0]);
  std::string src2_str = operand_to_string(instr.operands[1]);

  if (dest_reg_str != src1_str || src1_str.front() == '[') {
    emit("mov " + dest_reg_str + ", " + src1_str);
  }
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
  if (dest_reg_str != src_str || src_str.front() == '[') {
    emit("mov " + dest_reg_str + ", " + src_str);
  }
  emit("neg " + dest_reg_str);
}

void X86_64CodeGenerator::handle_logical_not(const IRInstruction& instr) {
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src_str = operand_to_string(instr.operands[0]);

  if (dest_reg_str != src_str || src_str.front() == '[') {
    emit("mov " + dest_reg_str + ", " + src_str);
  }
  emit("xor " + dest_reg_str + ", 1");
}

void X86_64CodeGenerator::handle_logical_and_or(
    const IRInstruction& instr, const std::string& op_mnemonic) {
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));

  std::string src1_str = operand_to_string(instr.operands[0]);
  std::string src2_str = operand_to_string(instr.operands[1]);
  if (dest_reg_str != src1_str || src1_str.front() == '[') {
    emit("mov " + dest_reg_str + ", " + src1_str);
  }
  emit(op_mnemonic + " " + dest_reg_str + ", " + src2_str);
}

void X86_64CodeGenerator::handle_cmp(const IRInstruction& instr) {
  // Result of CMP is boolean, stored in dest_reg
  // Operands are src1, src2
  std::string dest_reg_str =
      get_x86_reg(std::get<IR_Register>(instr.result.value()));
  std::string src1_str = operand_to_string(instr.operands[0]);
  std::string src2_str = operand_to_string(instr.operands[1]);

  // If both operands are memory, one must be moved to a register first.
  if (src1_str.front() == '[' && src1_str.back() == ']' &&
      src2_str.front() == '[' && src2_str.back() == ']') {
    std::string temp_reg = get_x86_reg(IR_Register(m_next_available_reg_idx++));
    emit("mov " + temp_reg + ", " + src1_str);
    src1_str = temp_reg;
  }

  bool rax_pushed = false;
  if (dest_reg_str != "rax") {
    emit("push rax");
    rax_pushed = true;
  }

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
  if (rax_pushed) emit("pop rax");
}

void X86_64CodeGenerator::handle_label(const IRInstruction& instr) {
  emit_label(operand_to_string(instr.result.value()));
}

void X86_64CodeGenerator::handle_goto(const IRInstruction& instr) {
  emit("jmp " + operand_to_string(instr.operands[0]));
}

void X86_64CodeGenerator::handle_if_z(const IRInstruction& instr) {
  // instr.operands[0] is the condition operand
  // instr.operands[1] is the target label
  std::string cond_op_str = operand_to_string(instr.operands[0]);
  std::string target_label_str = operand_to_string(instr.operands[1]);
  emit("test " + cond_op_str + ", " + cond_op_str); // Test if zero
  emit("jz " + target_label_str);                   // Jump if Zero (ZF=1)
}

void X86_64CodeGenerator::handle_push_param(const IRInstruction& instr) {
  std::string src_str = operand_to_string(instr.operands[0]);
  emit("push " + src_str);
}

void X86_64CodeGenerator::handle_pop_params(const IRInstruction& instr) {
  uint64_t num_bytes = std::get<IR_Immediate>(instr.operands[0]).val;
  if (num_bytes > 0) {
    emit("add rsp, " + std::to_string(num_bytes));
  }
}

void X86_64CodeGenerator::handle_lcall(const IRInstruction& instr) {
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
}

void X86_64CodeGenerator::handle_asm_block(const IRInstruction& instr) {
  assert(!instr.operands.empty() &&
         std::holds_alternative<std::string>(instr.operands[0]) &&
         "std::string operand must exist for ASM_BLOCK in IR");

  const std::string& asm_code = std::get<std::string>(instr.operands[0]);
  *m_out << asm_code << std::endl;
}
