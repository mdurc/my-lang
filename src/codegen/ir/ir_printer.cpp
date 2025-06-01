#include "ir_printer.h"

#include <iomanip>
#include <variant>

void print_ir_register(const IR_Register& reg, std::ostream& out) {
  out << "r" << reg.id;
}

void print_ir_immediate(const IR_Immediate& imm, std::ostream& out) {
  out << imm.val;
}

void print_ir_label(const IR_Label& label, std::ostream& out) {
  out << "L" << label.id;
}

void print_ir_operand(const IROperand& operand, std::ostream& out) {
  if (std::holds_alternative<IR_Register>(operand)) {
    print_ir_register(std::get<IR_Register>(operand), out);
  } else if (std::holds_alternative<IR_Immediate>(operand)) {
    print_ir_immediate(std::get<IR_Immediate>(operand), out);
  } else if (std::holds_alternative<IR_Label>(operand)) {
    print_ir_label(std::get<IR_Label>(operand), out);
  } else if (std::holds_alternative<std::string>(operand)) {
    // ASM_BLOCK
    out << "\"" << std::get<std::string>(operand) << "\"";
  }
}

void print_ir_instruction(const IRInstruction& instr, std::ostream& out) {
  switch (instr.opcode) {
    case IROpCode::ADD: out << "ADD    "; break;
    case IROpCode::SUB: out << "SUB    "; break;
    case IROpCode::MUL: out << "MUL    "; break;
    case IROpCode::DIV: out << "DIV    "; break;
    case IROpCode::MOD: out << "MOD    "; break;
    case IROpCode::CMP_EQ: out << "CMP_EQ "; break;
    case IROpCode::CMP_NE: out << "CMP_NE "; break;
    case IROpCode::CMP_LT: out << "CMP_LT "; break;
    case IROpCode::CMP_LE: out << "CMP_LE "; break;
    case IROpCode::CMP_GT: out << "CMP_GT "; break;
    case IROpCode::CMP_GE: out << "CMP_GE "; break;
    case IROpCode::AND: out << "AND    "; break;
    case IROpCode::OR: out << "OR     "; break;
    case IROpCode::NOT: out << "NOT    "; break;
    case IROpCode::MOV: out << "MOV    "; break;
    case IROpCode::NEG: out << "NEG    "; break;
    case IROpCode::FUNC: out << "FUNC   "; break;
    case IROpCode::LABEL: out << "LABEL  "; break;
    case IROpCode::GOTO: out << "GOTO   "; break;
    case IROpCode::GOTO_T: out << "GOTO_T "; break;
    case IROpCode::GOTO_F: out << "GOTO_F "; break;
    case IROpCode::PARAM: out << "PARAM  "; break;
    case IROpCode::CALL: out << "CALL   "; break;
    case IROpCode::RET: out << "RET    "; break;
    case IROpCode::READ: out << "READ   "; break;
    case IROpCode::PRINT: out << "PRINT  "; break;
    case IROpCode::ASM_BLOCK: out << "ASMBLK "; break;
    default: out << "UNKNOWN_OP "; break;
  }

  if (instr.result.has_value()) {
    print_ir_operand(instr.result.value(), out);
  }

  bool first_operand = true;
  if (instr.result.has_value() && !instr.operands.empty()) {
    out << ", ";
  }

  for (const IROperand& operand : instr.operands) {
    if (!first_operand) {
      out << ", ";
    }
    print_ir_operand(operand, out);
    first_operand = false;
  }
  out << std::endl;
}

void print_ir_instructions(const std::vector<IRInstruction>& instructions,
                           std::ostream& out) {
  out << "--- IR Instructions ---" << std::endl;
  if (instructions.empty()) {
    out << "(No instructions generated)" << std::endl;
  }
  for (const IRInstruction& instr : instructions) {
    print_ir_instruction(instr, out);
  }
  out << "-----------------------" << std::endl;
}
