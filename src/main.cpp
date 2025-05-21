#include <iostream>
#include <string>

#include "lexer/lexer.h"

int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <file_name>" << std::endl;
    return 1;
  }

  const std::string& filename = argv[1];
  try {
    Lexer lexer(filename);
    std::cout << lexer;
  } catch (const std::exception& e) {
    std::cerr << "Lexing failed: " << e.what() << std::endl;
    return 1;
  }

  return 0;
}
