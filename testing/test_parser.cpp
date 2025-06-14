#include <gtest/gtest.h>

#include "../src/lexer/lexer.h"
#include "../src/parser/parser.h"
#include "../src/parser/visitor.h"
#include "util.h"
#include "vendor/ApprovalTests.hpp"

std::string generate_parser_output(const std::string& input_filepath) {
  std::stringstream ss;
  Lexer lexer;
  try {
    const std::vector<Token>& tokens = lexer.tokenize(input_filepath);

    SymTab symtab;
    Parser parser;
    try {
      std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
      print_ast(ast, ss);
    } catch (const std::exception& e) {
      ss << "PARSER_EXCEPTION: " << e.what() << std::endl;
    }
  } catch (const std::exception& e) {
    ss << "LEXER_EXCEPTION: " << e.what() << std::endl;
  }

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
