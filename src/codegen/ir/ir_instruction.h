#ifndef CODEGEN_IR_IR_INSTRUCTION_H
#define CODEGEN_IR_IR_INSTRUCTION_H

#include <cassert>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "../../parser/types.h"

enum class IROpCode {
  BEGIN_FUNC, // Result: label_func_name, no operands
  END_FUNC,   // Operands: optional return value
  EXIT,       // Operands: exit code

  // Assignment and Data
  ASSIGN, // Result: dest_var_or_temp, Operands: src_operand
  LOAD,   // Result: dest_temp, Operands: address_operand (*addr)
  STORE,  // Result: address_dest, Operands: src_temp

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
  CMP_GE,     // Result: dest_temp, Operands: src1, src2
  CMP_STR_EQ, // Result: dest_temp (bool), Operands: str_ptr1, str_ptr2

  // Control Flow
  LABEL, // Result: label_operand (defines the label)
  GOTO,  // Operands: target_label_operand
  IF_Z,  // Operands: cond_operand, target_label_operand (IfZero/IfFalse)

  // Procedure calls
  PUSH_ARG, // Operands: src_operand
  LCALL,    // Result: opt_dst, Operands: func_label_operand/func ptr variable

  ASM_BLOCK,

  // Pointer & Memory
  ADDR_OF,     // Result: dest_reg, Operands: src_value
  ALLOC,       // Result: dest_ptr_reg, Operands: [size, ?initializer_op].
               // instr.size = initializer_op's type size if present
  ALLOC_ARRAY, // Result: dest_ptr_reg, Operands: [size_el, num_el].
               // instr.size = num_el's type size
  FREE,        // Operands: ptr_op
};

// == Operand Types ==

// Temporary registers: _t0, _t1
struct IR_Register {
  int id;

  IR_Register(int id = 0) : id(id) {}
  bool operator==(const IR_Register& other) const { return id == other.id; }
  bool operator<(const IR_Register& other) const { return id < other.id; }
};

struct IR_ParameterSlot {
  size_t index;     // 0-indexed param number
  size_t param_amt; // number of parameters in this group
  uint64_t size;    // size in bytes

  IR_ParameterSlot(size_t idx, size_t p_amt, uint64_t s)
      : index(idx), param_amt(p_amt), size(s) {}
  bool operator==(const IR_ParameterSlot& other) const {
    return index == other.index && size == other.size;
  }
  bool operator<(const IR_ParameterSlot& other) const {
    if (index != other.index) return index < other.index;
    return size < other.size;
  }
};

// Source-level variable from code
struct IR_Variable {
  std::string name;
  uint64_t size; // size in bytes
  bool is_func_decl;

  IR_Variable(const std::string& name, uint64_t size, bool func = false)
      : name(name), size(size), is_func_decl(func) {}
  bool operator==(const IR_Variable& other) const { return name == other.name; }
  bool operator<(const IR_Variable& other) const { return name < other.name; }
};

// Immediate integer values
struct IR_Immediate {
  uint64_t val;  // imms are all positive, though can be negated
  uint64_t size; // size in bytes

  IR_Immediate(uint64_t val, uint64_t s) : val(val), size(s) {}
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
using IROperand = std::variant<IR_Register, IR_Variable, IR_ParameterSlot,
                               IR_Immediate, IR_Label, std::string>;

struct IRInstruction {
  IROpCode opcode;
  // `result` is typically the destination operand (ASSIGN, ADD, LOAD, LCALL).
  // For LABEL or BEGIN_FUNC, it's the label being defined/named.
  std::optional<IROperand> result;
  //`operands` are the source operands or other parameters for the instruction.
  std::vector<IROperand> operands;

  // size is tracked in instructions as instructions often require a matching
  // size on both operands, and we enforce that this way.
  uint64_t size; // size in bytes

  IRInstruction(IROpCode op, std::optional<IROperand> res = std::nullopt,
                std::vector<IROperand> ops = {}, uint64_t s = 0)
      : opcode(op), result(std::move(res)), operands(std::move(ops)), size(s) {}
};

#endif // CODEGEN_IR_IR_INSTRUCTION_H
