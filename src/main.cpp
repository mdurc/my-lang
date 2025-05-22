#include <iostream>
#include <string>

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
    std::cout << lexer; // prints out last tokenized file

    SymTab symtab;

    Parser parser;
    std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);

    std::cout << "parsed: " << ast.size() << std::endl;
  } catch (const std::exception& e) {
    std::cerr << e.what() << std::endl;
    return 1;
  }

  return 0;
}
