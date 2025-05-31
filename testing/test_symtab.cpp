#include <gtest/gtest.h>

#include "../src/lexer/lexer.h"
#include "../src/parser/parser.h"
#include "util.h"
#include "vendor/ApprovalTests.hpp"

std::string generate_symtab_output(const std::string& input_filepath) {
  std::stringstream ss;
  Lexer lexer;
  try {
    const std::vector<Token>& tokens = lexer.tokenize(input_filepath);

    SymTab symtab;
    Parser parser;
    try {
      std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
      symtab.print(ss);
    } catch (const std::exception& e) {
      ss << "PARSER_EXCEPTION: " << e.what() << std::endl;
    }
  } catch (const std::exception& e) {
    ss << "LEXER_EXCEPTION: " << e.what() << std::endl;
  }

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

TEST(SymTabTests, SymTabMemory) {
  ApprovalTests::Approvals::verify(
      generate_symtab_output("./samples/memory.sn"));
}

TEST(SymTabTests, SymTabAsmAndErrors) {
  ApprovalTests::Approvals::verify(
      generate_symtab_output("./samples/asm_and_errors.sn"));
}

TEST(SymTabTests, SymTabRead) {
  ApprovalTests::Approvals::verify(generate_symtab_output("./samples/read.sn"));
}
