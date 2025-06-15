#include <gtest/gtest.h>

#include "../src/driver.h"
#include "../vendor/ApprovalTests.hpp"
#include "util.h"

std::string generate_symtab_output(const std::string& input_filepath) {
  std::stringstream ss;
  compile_symtab(input_filepath, ss);
  return rtrim(ss.str());
}

TEST(SymTabTests, SymTabVarDecl) {
  ApprovalTests::Approvals::verify(
      generate_symtab_output("./samples/var_decl.sn"));
}

TEST(SymTabTests, SymTabControlFlow) {
  ApprovalTests::Approvals::verify(
      generate_symtab_output("./samples/control_flow.sn"));
}

TEST(SymTabTests, SymTabFunctions) {
  ApprovalTests::Approvals::verify(
      generate_symtab_output("./samples/functions.sn"));
}

TEST(SymTabTests, SymTabStructs) {
  ApprovalTests::Approvals::verify(
      generate_symtab_output("./samples/structs.sn"));
}

TEST(SymTabTests, SymTabPointers) {
  ApprovalTests::Approvals::verify(
      generate_symtab_output("./samples/pointers.sn"));
}

TEST(SymTabTests, SymTabAsmAndErrors) {
  ApprovalTests::Approvals::verify(
      generate_symtab_output("./samples/asm_and_errors.sn"));
}

TEST(SymTabTests, SymTabRead) {
  ApprovalTests::Approvals::verify(generate_symtab_output("./samples/read.sn"));
}
