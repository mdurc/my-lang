#include <gtest/gtest.h>

#include "../src/lexer/lexer.h"
#include "util.h"
#include "vendor/ApprovalTests.hpp"

std::string generate_lexer_output(const std::string& input_filepath) {
  std::stringstream ss;
  Lexer lexer;
  try {
    std::vector<Token> tokens = lexer.tokenize(input_filepath);
    print_tokens(tokens, ss);
  } catch (const std::exception& e) {
    ss << "LEXER_EXCEPTION: " << e.what() << std::endl;
  }
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

TEST(LexerTests, LexerRead) {
  ApprovalTests::Approvals::verify(generate_lexer_output("./samples/read.sn"));
}
