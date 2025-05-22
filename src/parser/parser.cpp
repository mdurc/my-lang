#include "parser.h"

Parser::Parser(SymTab* symtab, std::vector<Token> tokens)
    : m_panic_mode(false),
      m_symtab(symtab),
      m_tokens(std::move(tokens)),
      m_pos(0) {
  // parse_program();
}
