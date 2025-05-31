#include <gtest/gtest.h>

#include "../src/checker/typechecker.h"
#include "../src/codegen/ir/ir_printer.h"
#include "../src/codegen/ir/ir_visitor.h"
#include "../src/lexer/lexer.h"
#include "../src/parser/parser.h"
#include "util.h"
#include "vendor/ApprovalTests.hpp"

std::string generate_ir_output(const std::string& input_filepath) {
  std::stringstream ss;
  Lexer lexer;
  try {
    const std::vector<Token>& tokens = lexer.tokenize(input_filepath);

    SymTab symtab;
    Parser parser;
    TypeChecker type_checker;
    IrVisitor ir_visitor;
    try {
      std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
      try {
        type_checker.check_program(&symtab, ast);

        try {
          ir_visitor.visit_all(ast);
          print_ir_instructions(ir_visitor.getInstructions(), ss);
        } catch (const std::exception& e) {
          ss << "IR_EXCEPTION: " << e.what() << std::endl;
        }
      } catch (const std::exception& e) {
        ss << "TYPE_CHECKER_EXCEPTION: " << e.what() << std::endl;
      }
    } catch (const std::exception& e) {
      ss << "PARSER_EXCEPTION: " << e.what() << std::endl;
    }
  } catch (const std::exception& e) {
    ss << "LEXER_EXCEPTION: " << e.what() << std::endl;
  }

  return rtrim(ss.str());
}

TEST(IrTests, IrArithmetic) {
  ApprovalTests::Approvals::verify(
      generate_ir_output("./ir-samples/arithmetic.sn"));
}

TEST(IrTests, IrLoops) {
  ApprovalTests::Approvals::verify(generate_ir_output("./ir-samples/loops.sn"));
}

TEST(IrTests, IrFuncs) {
  ApprovalTests::Approvals::verify(generate_ir_output("./ir-samples/funcs.sn"));
}

/*
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

TEST(IrTests, IrMemory) {
  ApprovalTests::Approvals::verify(generate_ir_output("./samples/memory.sn"));
}

TEST(IrTests, IrAsmAndErrors) {
  ApprovalTests::Approvals::verify(
      generate_ir_output("./samples/asm_and_errors.sn"));
}

TEST(IrTests, IrRead) {
  ApprovalTests::Approvals::verify(generate_ir_output("./samples/read.sn"));
}
*/
