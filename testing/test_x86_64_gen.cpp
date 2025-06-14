#include <gtest/gtest.h>

#include "../src/checker/typechecker.h"
#include "../src/codegen/ir/ir_visitor.h"
#include "../src/codegen/x86_64/asm.h"
#include "../src/lexer/lexer.h"
#include "../src/parser/parser.h"
#include "util.h"
#include "vendor/ApprovalTests.hpp"

std::string generate_x86gen_output(const std::string& input_filepath) {
  std::stringstream ss;
  Lexer lexer;
  try {
    const std::vector<Token>& tokens = lexer.tokenize(input_filepath);

    SymTab symtab;
    Parser parser;
    TypeChecker type_checker;
    IrVisitor ir_visitor(&symtab);
    try {
      std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
      try {
        type_checker.check_program(&symtab, ast);

        try {
          ir_visitor.visit_all(ast);
          const std::vector<IRInstruction>& instrs =
              ir_visitor.get_instructions();
          try {
            X86_64CodeGenerator gen;
            gen.generate(instrs, ir_visitor.is_main_defined(), ss);
          } catch (const std::exception& e) {
            ss << "CODEGEN_EXCEPTION: " << e.what() << std::endl;
          }
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

TEST(IrTests, IrMemory) {
  ApprovalTests::Approvals::verify(generate_x86gen_output("./samples/memory.sn"));
}

TEST(IrTests, IrAsmAndErrors) {
  ApprovalTests::Approvals::verify(
      generate_x86gen_output("./samples/asm_and_errors.sn"));
}

TEST(IrTests, IrRead) {
  ApprovalTests::Approvals::verify(generate_x86gen_output("./samples/read.sn"));
}
*/
