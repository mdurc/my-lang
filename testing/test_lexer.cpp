#include <gtest/gtest.h>

#include "../src/driver.h"
#include "../vendor/ApprovalTests.hpp"
#include "util.h"

std::string generate_lexer_output(const std::string& input_filepath) {
  std::stringstream ss;
  compile_tokens(input_filepath, ss);
  return rtrim(ss.str());
}

TEST(LexerTests, LexerVarDecl) {
  ApprovalTests::Approvals::verify(
      generate_lexer_output("./samples/var_decl.sn"));
}

TEST(LexerTests, LexerControlFlow) {
  ApprovalTests::Approvals::verify(
      generate_lexer_output("./samples/control_flow.sn"));
}

TEST(LexerTests, LexerFunctions) {
  ApprovalTests::Approvals::verify(
      generate_lexer_output("./samples/functions.sn"));
}

TEST(LexerTests, LexerStructs) {
  ApprovalTests::Approvals::verify(
      generate_lexer_output("./samples/structs.sn"));
}
TEST(LexerTests, LexerPointers) {
  ApprovalTests::Approvals::verify(
      generate_lexer_output("./samples/pointers.sn"));
}

TEST(LexerTests, LexerAsmAndErrors) {
  ApprovalTests::Approvals::verify(
      generate_lexer_output("./samples/asm_and_errors.sn"));
}

TEST(LexerTests, LexerStdin) {
  ApprovalTests::Approvals::verify(generate_lexer_output("./samples/stdin.sn"));
}
