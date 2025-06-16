#include <gtest/gtest.h>

#include "../src/driver.h"
#include "../vendor/ApprovalTests.hpp"
#include "util.h"

std::string generate_x86gen_output(const std::string& input_filepath) {
  std::stringstream ss;
  compile_asm(input_filepath, ss);
  return rtrim(ss.str());
}

TEST(x86GenTests, x86GenVarDecl) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./samples/var_decl.sn"));
}

TEST(x86GenTests, x86GenControlFlow) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./samples/control_flow.sn"));
}

TEST(x86GenTests, x86GenFunctions) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./samples/functions.sn"));
}

TEST(x86GenTests, x86GenStructs) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./samples/structs.sn"));
}

TEST(x86GenTests, x86GenPointers) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./samples/pointers.sn"));
}

TEST(x86GenTests, x86GenAsmAndErrors) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./samples/asm_and_errors.sn"));
}

TEST(x86GenTests, x86GenStdin) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./samples/stdin.sn"));
}
