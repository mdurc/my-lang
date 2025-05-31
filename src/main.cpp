#include <iostream>
#include <string>

#include "checker/typechecker.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "parser/symtab.h"
#include "parser/visitor.h"

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

    TypeChecker type_checker(&symtab);
    type_checker.check_program(ast);

  } catch (const std::exception& e) {
    std::cerr << e.what() << std::endl;
    return 1;
  }

  return 0;
}
