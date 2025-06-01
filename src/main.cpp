#include <iostream>
#include <string>

#include "checker/typechecker.h"
#include "codegen/ir/ir_printer.h"
#include "codegen/ir/ir_visitor.h"
#include "codegen/x86_64/asm.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "parser/symtab.h"

int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <file_name>" << std::endl;
    return 1;
  }

  const std::string& filename = argv[1];

  try {
    Lexer lexer;
    const std::vector<Token>& tokens = lexer.tokenize(filename);
    // print_tokens(tokens, std::cout);

    SymTab symtab;

    Parser parser;
    std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
    // print_ast(ast, std::cout);

    // symtab.print(std::cout);

    TypeChecker type_checker;
    type_checker.check_program(&symtab, ast);

    IrVisitor ir_visitor;
    ir_visitor.visit_all(ast);
    const std::vector<IRInstruction>& instrs = ir_visitor.get_instructions();
    // print_ir_instructions(instrs, std::cout);

    X86_64CodeGenerator gen;
    gen.generate(instrs, std::cout);

  } catch (const std::exception& e) {
    std::cerr << e.what() << std::endl;
    return 1;
  }

  return 0;
}
