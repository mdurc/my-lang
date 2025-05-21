#ifndef LEXER_LEXER_H
#define LEXER_LEXER_H

#include <map>
#include <string>
#include <vector>

#include "../logging/logger.h"
#include "token.h"

class Lexer {
public:
  Lexer(const std::string& filename);

  const std::vector<Token>& get_tokens() const { return m_tokens; }

  friend std::ostream& operator<<(std::ostream& out, const Lexer& lex);

private:
  Logger m_logger;

  std::string m_source;
  size_t m_lex_start;
  size_t m_lex_pos;

  std::vector<Token> m_tokens;
  int m_row;
  int m_col;
  int m_start_col;

  static const std::map<std::string, TokenType> s_keyword_map;

  void tokenize();
  void scan_token();

  bool is_at_end() const;
  char advance();
  char peek() const;
  char peek_next() const;
  bool match(char expected);

  void add_token(TokenType type, Token::Lit literal_value = std::monostate{});

  void lex_string();
  void lex_number();
  void lex_identifier_or_keyword();
  void skip_whitespace_and_comments();
};

#endif // LEXER_LEXER_H
