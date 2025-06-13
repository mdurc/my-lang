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
  if (argc < 2) {
    std::cerr << "Usage: " << argv[0]
              << " <file_name> [--tokens] [--ast] [--symtab] [--ir] [--gen]"
              << std::endl;
    return 1;
  }

  const std::string& filename = argv[1];

  bool print_tokens_flag = false;
  bool print_ast_flag = false;
  bool print_symtab_flag = false;
  bool print_ir_flag = false;
  bool generate_code_flag = false;

  for (int i = 2; i < argc; ++i) {
    std::string arg = argv[i];
    if (arg == "--tokens")
      print_tokens_flag = true;
    else if (arg == "--ast")
      print_ast_flag = true;
    else if (arg == "--symtab")
      print_symtab_flag = true;
    else if (arg == "--ir")
      print_ir_flag = true;
    else if (arg == "--gen")
      generate_code_flag = true;
    else {
      std::cerr << "Unknown option: " << arg << std::endl;
      return 1;
    }
  }

  try {
    Lexer lexer;
    const std::vector<Token>& tokens = lexer.tokenize(filename);
    if (print_tokens_flag) {
      print_tokens(tokens, std::cout);
    }

    SymTab symtab;

    Parser parser;
    std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
    if (print_ast_flag) {
      print_ast(ast, std::cout);
    }

    if (print_symtab_flag) {
      symtab.print(std::cout);
    }

    TypeChecker type_checker;
    type_checker.check_program(&symtab, ast);

    IrVisitor ir_visitor(&symtab);
    ir_visitor.visit_all(ast);
    const std::vector<IRInstruction>& instrs = ir_visitor.get_instructions();
    if (print_ir_flag) {
      print_ir_instructions(instrs, std::cout);
    }

    if (generate_code_flag) {
      X86_64CodeGenerator gen;
      gen.generate(instrs, ir_visitor.is_main_defined(), std::cout);
    }

  } catch (const std::exception& e) {
    std::cerr << e.what() << std::endl;
    return 1;
  }

  return 0;
}
