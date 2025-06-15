#include <gtest/gtest.h>

#include "../src/driver.h"
#include "../vendor/ApprovalTests.hpp"
#include "util.h"

std::string generate_parser_output(const std::string& input_filepath) {
  std::stringstream ss;
  compile_ast(input_filepath, ss);
  return rtrim(ss.str());
}

TEST(ParserTests, ParserVarDecl) {
  ApprovalTests::Approvals::verify(
      generate_parser_output("./samples/var_decl.sn"));
}

TEST(ParserTests, ParserControlFlow) {
  ApprovalTests::Approvals::verify(
      generate_parser_output("./samples/control_flow.sn"));
}

TEST(ParserTests, ParserFunctions) {
  ApprovalTests::Approvals::verify(
      generate_parser_output("./samples/functions.sn"));
}

TEST(ParserTests, ParserStructs) {
  ApprovalTests::Approvals::verify(
      generate_parser_output("./samples/structs.sn"));
}

TEST(ParserTests, ParserPointers) {
  ApprovalTests::Approvals::verify(
      generate_parser_output("./samples/pointers.sn"));
}

TEST(ParserTests, ParserAsmAndErrors) {
  ApprovalTests::Approvals::verify(
      generate_parser_output("./samples/asm_and_errors.sn"));
}

TEST(ParserTests, ParserRead) {
  ApprovalTests::Approvals::verify(generate_parser_output("./samples/read.sn"));
}
