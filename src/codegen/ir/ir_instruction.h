#ifndef CODEGEN_IR_IR_INSTRUCTION_H
#define CODEGEN_IR_IR_INSTRUCTION_H

#include <cassert>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

enum class IROpCode {
  BEGIN_FUNC, // Result: label_func_name, Operands: imm_stack_size
  END_FUNC,   // No operands
  EXIT,       // No operands

  // Assignment and Data
  ASSIGN, // Result: dest_var_or_temp, Operands: src_operand
  LOAD,   // Result: dest_temp, Operands: address_operand (*addr)
  STORE,  // Operands: address_operand, src_operand (*addr = val)

  // Arithmetic / Logical (dest = src1 op src2)

  ADD,
  SUB,
  MUL,
  DIV,
  MOD, // Result: dest_temp, Operands: src1, src2
  NEG, // Result: dest_temp, Operands: src
  AND,
  OR,
  NOT, // NOT is unary (dest, src), AND/OR are binary (dest, src1, src2)

  // Comparison (dest = src1 op src2) -> result is bool (0 or 1)
  CMP_EQ,
  CMP_NE,
  CMP_LT,
  CMP_LE,
  CMP_GT,
  CMP_GE, // Result: dest_temp, Operands: src1, src2

  // Control Flow
  LABEL, // Result: label_operand (defines the label)
  GOTO,  // Operands: target_label_operand
  IF_Z,  // Operands: cond_operand, target_label_operand (IfZero/IfFalse)

  // Procedure calls
  PUSH_ARG, // Operands: src_operand
  POP_ARGS, // Operands: imm_num_bytes
  LCALL,    // Result: opt_dest_temp, Operands: func_label_operand
  RETURN,   // Operands: opt_val_operand

  ASM_BLOCK,
};

// == Operand Types ==

// Temporary registers: _t0, _t1
struct IR_Register {
  int id;

  IR_Register(int id = 0) : id(id) {}
  bool operator==(const IR_Register& other) const { return id == other.id; }
  bool operator<(const IR_Register& other) const { return id < other.id; }
};

// Source-level variable from code
struct IR_Variable {
  std::string name;
  IR_Variable(const std::string& n) : name(n) {}

  bool operator==(const IR_Variable& other) const { return name == other.name; }
  bool operator<(const IR_Variable& other) const { return name < other.name; }
};

// Immediate integer values
struct IR_Immediate {
  uint64_t val; // imms are all positive, though can be negated

  IR_Immediate(uint64_t val = 0) : val(val) {}
  bool operator==(const IR_Immediate& other) const { return val == other.val; }
  bool operator<(const IR_Immediate& other) const { return val < other.val; }
};

// Label: _L0, _foo
struct IR_Label {
  int id;
  std::string name;

  IR_Label(int id = 0) : id(id), name("_L" + std::to_string(id)) {}
  IR_Label(const std::string& n) : id(-1), name(n) {}

  bool operator==(const IR_Label& other) const { return id == other.id; }
  bool operator<(const IR_Label& other) const {
    if (id != other.id) return id < other.id;
    return name < other.name;
  }
};

// string for ASM_BLOCK, string literals, etc.
using IROperand =
    std::variant<IR_Register, IR_Variable, IR_Immediate, IR_Label, std::string>;

struct IRInstruction {
  IROpCode opcode;
  // `result` is typically the destination operand (ASSIGN, ADD, LOAD, LCALL).
  // For LABEL or BEGIN_FUNC, it's the label being defined/named.
  std::optional<IROperand> result;
  //`operands` are the source operands or other parameters for the instruction.
  std::vector<IROperand> operands;

  IRInstruction(IROpCode op, std::optional<IROperand> res = std::nullopt,
                std::vector<IROperand> ops = {})
      : opcode(op), result(std::move(res)), operands(std::move(ops)) {}
};

#endif // CODEGEN_IR_IR_INSTRUCTION_H
