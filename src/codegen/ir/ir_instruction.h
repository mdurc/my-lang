#ifndef CODEGEN_IR_IR_INSTRUCTION_H
#define CODEGEN_IR_IR_INSTRUCTION_H

#include <optional>
#include <string>
#include <variant>
#include <vector>

enum class IROpCode {
  // Arithmetic: dest = src1 op src2
  ADD,
  SUB,
  MUL,
  DIV,

  // Data movement: dest = src1
  MOV, // src1 can be immediate or register
};

struct IR_Register {
  int id;

  IR_Register() : id(0) {}
  IR_Register(int id) : id(id) {}
  bool operator==(const IR_Register& other) const { return id == other.id; }
  bool operator<(const IR_Register& other) const { return id < other.id; }
};

struct IR_Immediate {
  uint64_t val; // imms are all positive, though can be negated using 0 - imm

  IR_Immediate(int val) : val(val) {}
  bool operator==(const IR_Immediate& other) const { return val == other.val; }
  bool operator<(const IR_Immediate& other) const { return val < other.val; }
};

using IROperand = std::variant<IR_Register, IR_Immediate>;

struct IRInstruction {
  IROpCode opcode;
  IR_Register dest;              // always a register
  IROperand src1;                // can be a register or an immediate
  std::optional<IROperand> src2; // for some instructions such as MOV

  IRInstruction(IROpCode op, IR_Register d, IROperand s1, IROperand s2_val)
      : opcode(op), dest(d), src1(s1), src2(s2_val) {}
  IRInstruction(IROpCode op, IR_Register d, IROperand s1)
      : opcode(op), dest(d), src1(s1), src2(std::nullopt) {}
};

#endif // CODEGEN_IR_IR_INSTRUCTION_H
