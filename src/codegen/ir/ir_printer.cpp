#include "ir_printer.h"

#include <variant>

void print_ir_register(const IR_Register& reg, std::ostream& out) {
  out << "_t" << reg.id;
}

void print_ir_variable(const IR_Variable& var, std::ostream& out) {
  out << var.name;
}

void print_ir_immediate(const IR_Immediate& imm, std::ostream& out) {
  out << imm.val;
}

void print_ir_label(const IR_Label& label, std::ostream& out) {
  out << label.name;
}

void print_ir_operand(const IROperand& operand, std::ostream& out) {
  if (std::holds_alternative<IR_Register>(operand)) {
    print_ir_register(std::get<IR_Register>(operand), out);
  } else if (std::holds_alternative<IR_Variable>(operand)) {
    print_ir_variable(std::get<IR_Variable>(operand), out);
  } else if (std::holds_alternative<IR_Immediate>(operand)) {
    print_ir_immediate(std::get<IR_Immediate>(operand), out);
  } else if (std::holds_alternative<IR_Label>(operand)) {
    print_ir_label(std::get<IR_Label>(operand), out);
  } else if (std::holds_alternative<std::string>(operand)) {
    out << "\"" << std::get<std::string>(operand) << "\"";
  }
}

void print_ir_instruction(const IRInstruction& instr, std::ostream& out) {
  // Labels are not indented
  if (instr.opcode == IROpCode::LABEL) {
    assert(instr.result.has_value());
    print_ir_operand(instr.result.value(), out);
    out << ":" << std::endl;
    return;
  }

  if (instr.opcode == IROpCode::BEGIN_FUNC) {
    assert(instr.result.has_value());
    print_ir_operand(instr.result.value(), out);
    out << " " << std::endl;
  }

  out << "\t";

  switch (instr.opcode) {
    case IROpCode::BEGIN_FUNC: out << "BeginFunc"; break;
    case IROpCode::END_FUNC: out << "EndFunc"; break;
    case IROpCode::EXIT: out << "Exit"; break;
    case IROpCode::ASSIGN:
      assert(instr.result.has_value() && instr.operands.size() == 1);
      print_ir_operand(instr.result.value(), out);
      out << " = ";
      print_ir_operand(instr.operands[0], out);
      break;
    case IROpCode::LOAD: // dest = *addr
      assert(instr.result.has_value() && instr.operands.size() == 1);
      print_ir_operand(instr.result.value(), out);
      out << " = *(";
      print_ir_operand(instr.operands[0], out);
      out << ")";
      break;
    case IROpCode::STORE: // *addr = val
      assert(instr.result.has_value() && instr.operands.size() == 1);
      out << "*(";
      print_ir_operand(instr.result.value(), out);
      out << ") = ";
      print_ir_operand(instr.operands[0], out);
      break;
    case IROpCode::ADD:
    case IROpCode::SUB:
    case IROpCode::MUL:
    case IROpCode::DIV:
    case IROpCode::MOD:
    case IROpCode::AND:
    case IROpCode::OR:
    case IROpCode::CMP_EQ:
    case IROpCode::CMP_NE:
    case IROpCode::CMP_LT:
    case IROpCode::CMP_LE:
    case IROpCode::CMP_GT:
    case IROpCode::CMP_GE:
      assert(instr.result.has_value() && instr.operands.size() == 2);
      print_ir_operand(instr.result.value(), out);
      out << " = ";
      print_ir_operand(instr.operands[0], out);
      switch (instr.opcode) {
        case IROpCode::ADD: out << " + "; break;
        case IROpCode::SUB: out << " - "; break;
        case IROpCode::MUL: out << " * "; break;
        case IROpCode::DIV: out << " / "; break;
        case IROpCode::MOD: out << " % "; break;
        case IROpCode::AND: out << " && "; break;
        case IROpCode::OR: out << " || "; break;
        case IROpCode::CMP_EQ: out << " == "; break;
        case IROpCode::CMP_NE: out << " != "; break;
        case IROpCode::CMP_LT: out << " < "; break;
        case IROpCode::CMP_LE: out << " <= "; break;
        case IROpCode::CMP_GT: out << " > "; break;
        case IROpCode::CMP_GE: out << " >= "; break;
        default: out << " ?? "; break;
      }
      print_ir_operand(instr.operands[1], out);
      break;
    case IROpCode::NEG:
    case IROpCode::NOT:
      assert(instr.result.has_value() && instr.operands.size() == 1);
      print_ir_operand(instr.result.value(), out);
      out << " = ";
      if (instr.opcode == IROpCode::NEG) out << "-";
      if (instr.opcode == IROpCode::NOT) out << "!";
      print_ir_operand(instr.operands[0], out);
      break;
    case IROpCode::GOTO:
      assert(!instr.result.has_value() && instr.operands.size() == 1);
      out << "Goto ";
      print_ir_operand(instr.operands[0], out);
      break;
    case IROpCode::IF_Z: // IfZ cond Goto Lbl
      assert(!instr.result.has_value() && instr.operands.size() == 2);
      out << "IfZ ";
      print_ir_operand(instr.operands[0], out);
      out << " Goto ";
      print_ir_operand(instr.operands[1], out);
      break;
    case IROpCode::PUSH_ARG:
      assert(!instr.result.has_value() && instr.operands.size() == 1);
      out << "PushArg ";
      print_ir_operand(instr.operands[0], out);
      break;
    case IROpCode::POP_ARGS:
      assert(!instr.result.has_value());
      out << "PopArgs";
      break;
    case IROpCode::LCALL: // opt_dest = LCall func_label
      assert(instr.operands.size() == 2);
      if (instr.result.has_value()) {
        print_ir_operand(instr.result.value(), out);
        out << " = ";
      }
      out << "LCall ";
      print_ir_operand(instr.operands[0], out);
      break;
    case IROpCode::RETURN:
      out << "Return";
      if (!instr.operands.empty()) {
        out << " ";
        print_ir_operand(instr.operands[0], out);
      }
      break;
    case IROpCode::ASM_BLOCK:
      assert(!instr.result.has_value() && instr.operands.size() == 1 &&
             std::holds_alternative<std::string>(instr.operands[0]));
      out << "Asm ";
      print_ir_operand(instr.operands[0], out);
      break;
    case IROpCode::LABEL: break;
    default:
      out << "UNKNOWN_IR_OPCODE(" << static_cast<int>(instr.opcode) << ")";
      break;
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
