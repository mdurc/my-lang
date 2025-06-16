#include <gtest/gtest.h>

#include "../src/driver.h"
#include "../vendor/ApprovalTests.hpp"
#include "util.h"

std::string generate_ir_output(const std::string& input_filepath) {
  std::stringstream ss;
  compile_ir(input_filepath, ss);
  return rtrim(ss.str());
}

TEST(IrTests, IrVarDecl) {
  ApprovalTests::Approvals::verify(generate_ir_output("./samples/var_decl.sn"));
}

TEST(IrTests, IrControlFlow) {
  ApprovalTests::Approvals::verify(
      generate_ir_output("./samples/control_flow.sn"));
}

TEST(IrTests, IrFunctions) {
  ApprovalTests::Approvals::verify(
      generate_ir_output("./samples/functions.sn"));
}

TEST(IrTests, IrStructs) {
  ApprovalTests::Approvals::verify(generate_ir_output("./samples/structs.sn"));
}

TEST(IrTests, IrPointers) {
  ApprovalTests::Approvals::verify(generate_ir_output("./samples/pointers.sn"));
}

TEST(IrTests, IrAsmAndErrors) {
  ApprovalTests::Approvals::verify(
      generate_ir_output("./samples/asm_and_errors.sn"));
}

TEST(IrTests, IrStdin) {
  ApprovalTests::Approvals::verify(generate_ir_output("./samples/stdin.sn"));
}
