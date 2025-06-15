#include <gtest/gtest.h>

#include "../src/driver.h"
#include "../vendor/ApprovalTests.hpp"
#include "util.h"

std::string generate_x86gen_output(const std::string& input_filepath) {
  std::stringstream ss;
  compile_asm(input_filepath, ss);
  return rtrim(ss.str());
}

TEST(x86GenTests, x86GenFizzBuzz) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./ir-samples/fizzbuzz.sn"));
}

TEST(x86GenTests, x86GenArithmetic) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./ir-samples/arithmetic.sn"));
}

TEST(x86GenTests, x86GenLoops) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./ir-samples/loops.sn"));
}

TEST(x86GenTests, x86GenFuncs) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./ir-samples/funcs.sn"));
}

TEST(x86GenTests, x86GenInOut) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./ir-samples/inout.sn"));
}

/*
TEST(IrTests, IrVarDecl) {
  ApprovalTests::Approvals::verify(generate_x86gen_output("./samples/var_decl.sn"));
}

TEST(IrTests, IrControlFlow) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./samples/control_flow.sn"));
}

TEST(IrTests, IrFunctions) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./samples/functions.sn"));
}

TEST(IrTests, IrStructs) {
  ApprovalTests::Approvals::verify(generate_x86gen_output("./samples/structs.sn"));
}

TEST(IrTests, IrPointers) {
  ApprovalTests::Approvals::verify(generate_x86gen_output("./samples/pointers.sn"));
}

TEST(IrTests, IrAsmAndErrors) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./samples/asm_and_errors.sn"));
}

TEST(IrTests, IrRead) {
  ApprovalTests::Approvals::verify(generate_x86gen_output("./samples/read.sn"));
}
*/
