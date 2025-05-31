#ifndef CODEGEN_IR_IR_INSTRUCTION_H
#define CODEGEN_IR_IR_INSTRUCTION_H

#include <optional>
#include <string>
#include <variant>
#include <vector>

enum class IROpCode {
  // dest = src1 op src2
  ADD,
  SUB,
  MUL,
  DIV,
  CMP_EQ,
  CMP_NE,
  CMP_LT,
  CMP_LE,
  CMP_GT,
  CMP_GE,
  AND,
  OR,

  // Data movement: dest = src1
  MOV, // src1 can be immediate or register

  // Unary: dest = op src1
  NEG,
  NOT,

  // Control Flow
  LABEL,  // LABEL L1 (defines L1)
  GOTO,   // GOTO L1
  GOTO_T, // GOTO_T cond_reg, L1 (Jump if true)
  GOTO_F, // GOTO_F cond_reg, L1 (Jump if false)

  // Procedure calls
  PARAM, // param src1
  CALL,  // call <param sources>
  RET    // Return from function

  // Subscript operator: x = y[i] and x[i] = y
  // Addressof (mut)? operator
  // Star operator: x = *y and *x = y
};

struct IR_Register {
  int id;

  IR_Register(int id = 0) : id(id) {}
  bool operator==(const IR_Register& other) const { return id == other.id; }
  bool operator<(const IR_Register& other) const { return id < other.id; }
};

struct IR_Immediate {
  uint64_t val; // imms are all positive, though can be negated using 0 - imm

  IR_Immediate(int val) : val(val) {}
  bool operator==(const IR_Immediate& other) const { return val == other.val; }
  bool operator<(const IR_Immediate& other) const { return val < other.val; }
};

struct IR_Label {
  int id;
  IR_Label(int id = 0) : id(id) {}
  bool operator==(const IR_Label& other) const { return id == other.id; }
  bool operator<(const IR_Label& other) const { return id < other.id; }
};

using IROperand = std::variant<IR_Register, IR_Immediate, IR_Label>;

struct IRInstruction {
  IROpCode opcode;
  std::optional<IROperand> result; // For instructions that produce a result or
                                   // define something (e.g. LABEL)
  std::vector<IROperand> operands; // Source operands, jump targets, etc.

  // LABEL Lbl; GOTO Lbl; PARAM val;
  IRInstruction(IROpCode op, IROperand val_for_result_or_operand) : opcode(op) {
    if (op == IROpCode::LABEL) {
      result = val_for_result_or_operand;
    } else if (op == IROpCode::GOTO || op == IROpCode::PARAM) {
      operands.push_back(val_for_result_or_operand);
    }
  }

  // MOV dest, src; NEG dest, src; NOT dest, src
  IRInstruction(IROpCode op, IR_Register res, IROperand op1)
      : opcode(op), result(res) {
    operands.push_back(op1);
  }

  // ADD dest, src1, src2 (binary ops)
  IRInstruction(IROpCode op, IR_Register res, IROperand op1, IROperand op2)
      : opcode(op), result(res) {
    operands.push_back(op1);
    operands.push_back(op2);
  }

  // GOTO_T/F cond, Lbl_target
  IRInstruction(IROpCode op, IROperand cond, IR_Label lbl_target) : opcode(op) {
    operands.push_back(cond);
    operands.push_back(lbl_target);
  }

  // CALL dest_opt, func_target, num_args_imm
  IRInstruction(IROpCode op, std::optional<IR_Register> dest_opt,
                IROperand func_target, IR_Immediate num_args)
      : opcode(op), result(std::move(dest_opt)) {
    operands.push_back(func_target);
    operands.push_back(num_args);
  }

  // RET (void)
  IRInstruction(IROpCode op) : opcode(op) {}
};

#endif // CODEGEN_IR_IR_INSTRUCTION_H
