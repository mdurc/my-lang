#include "ir_printer.h"

#include <variant>

void print_ir_register(const IR_Register& reg, std::ostream& out) {
  out << "r" << reg.id;
}

void print_ir_immediate(const IR_Immediate& imm, std::ostream& out) {
  out << imm.val;
}

void print_ir_operand(const IROperand& operand, std::ostream& out) {
  if (std::holds_alternative<IR_Register>(operand)) {
    print_ir_register(std::get<IR_Register>(operand), out);
  } else if (std::holds_alternative<IR_Immediate>(operand)) {
    print_ir_immediate(std::get<IR_Immediate>(operand), out);
  }
}

void print_ir_instruction(const IRInstruction& instr, std::ostream& out) {
  switch (instr.opcode) {
    case IROpCode::ADD: out << "ADD "; break;
    case IROpCode::SUB: out << "SUB "; break;
    case IROpCode::MUL: out << "MUL "; break;
    case IROpCode::DIV: out << "DIV "; break;
    case IROpCode::MOV: out << "MOV "; break;
    default: out << "UNKNOWN_OP "; break;
  }

  print_ir_register(instr.dest, out);
  out << ", ";
  print_ir_operand(instr.src1, out);

  if (instr.src2.has_value()) {
    out << ", ";
    print_ir_operand(instr.src2.value(), out);
  }
  out << std::endl;
}

void print_ir_instructions(const std::vector<IRInstruction>& instructions,
                           std::ostream& out) {
  out << "--- IR Instructions ---" << std::endl;
  if (instructions.empty()) {
    out << "(No instructions generated)" << std::endl;
  }
  for (const auto& instr : instructions) {
    print_ir_instruction(instr, out);
  }
  out << "-----------------------" << std::endl;
}
