#include "lexer.h"

#include <cctype>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>

const std::map<std::string, TokenType> Lexer::s_keyword_map = {
    {"func", TokenType::FUNC},     {"if", TokenType::IF},
    {"else", TokenType::ELSE},     {"for", TokenType::FOR},
    {"while", TokenType::WHILE},   {"print", TokenType::PRINT},
    {"return", TokenType::RETURN}, {"returns", TokenType::RETURNS},
    {"struct", TokenType::STRUCT}, {"switch", TokenType::SWITCH},
    {"case", TokenType::CASE},     {"default", TokenType::DEFAULT},
    {"break", TokenType::BREAK},   {"continue", TokenType::CONTINUE},
    {"mut", TokenType::MUT},       {"read", TokenType::READ},
    {"take", TokenType::TAKE},     {"give", TokenType::GIVE},
    {"ptr", TokenType::PTR},       {"imm", TokenType::IMM},
    {"new", TokenType::NEW},       {"free", TokenType::FREE},
    {"asm", TokenType::ASM},       {"Error", TokenType::ERROR_KEYWORD},
    {"true", TokenType::TRUE},     {"false", TokenType::FALSE},
    {"null", TokenType::NULL_},    {"and", TokenType::AND},
    {"or", TokenType::OR},         {"u0", TokenType::U0},
    {"u8", TokenType::U8},         {"u16", TokenType::U16},
    {"u32", TokenType::U32},       {"u64", TokenType::U64},
    {"i8", TokenType::I8},         {"i16", TokenType::I16},
    {"i32", TokenType::I32},       {"i64", TokenType::I64},
    {"f64", TokenType::F64},       {"bool", TokenType::BOOL},
    {"String", TokenType::STRING}};

void Lexer::setup() {
  m_logger.clear();

  m_source.clear();
  m_lex_start = 0;
  m_lex_pos = 0;

  m_tokens.clear();
  m_row = 1;
  m_col = 1;
  m_start_col = 1;
}

std::ostream& operator<<(std::ostream& out, const Lexer& lex) {
  out << "Tokens (" << lex.m_tokens.size() << "):" << '\n';
  for (const Token& token : lex.m_tokens) {
    out << token << '\n';
  }
  return out;
}

std::vector<Token> Lexer::tokenize(const std::string& filename) {
  setup();

  std::ifstream file(filename);
  if (!file.is_open()) {
    throw std::runtime_error("Could not open file: " + filename);
  }

  std::stringstream buffer;
  buffer << file.rdbuf();
  m_source = buffer.str();

  while (!is_at_end()) {
    skip_whitespace_and_comments();
    if (is_at_end()) break;

    m_start_col = m_col;
    m_lex_start = m_lex_pos;
    scan_token();
  }

  if (m_logger.output_diagnostics()) {
    throw std::runtime_error(
        "Lexing failed with " + std::to_string(m_logger.num_errors()) +
        " errors and " + std::to_string(m_logger.num_warnings()) +
        " warnings.");
  }

  return m_tokens;
}

bool Lexer::is_at_end() const { return m_lex_pos >= m_source.size(); }

char Lexer::advance() {
  if (is_at_end()) return '\0';
  char current_char = m_source[m_lex_pos++];
  if (current_char == '\n') {
    ++m_row;
    m_col = 1;
  } else {
    m_col++;
  }
  return current_char;
}

char Lexer::peek() const {
  if (is_at_end()) return '\0';
  return m_source[m_lex_pos];
}

char Lexer::peek_next() const {
  if (m_lex_pos + 1 >= m_source.size()) return '\0';
  return m_source[m_lex_pos + 1];
}

bool Lexer::match(char expected) {
  if (is_at_end() || m_source[m_lex_pos] != expected) {
    return false;
  }
  advance(); // consume the character
  return true;
}

// optional literal value
void Lexer::add_token(TokenType type, Token::Lit literal_value) {
  m_tokens.push_back(
      Token(type, m_source.substr(m_lex_start, m_lex_pos - m_lex_start),
            Span(m_row, m_start_col, m_col), std::move(literal_value)));
}

void Lexer::skip_whitespace_and_comments() {
  while (true) {
    char c = peek();
    if (isspace(c)) {
      advance();
    } else if (c == '/' && peek_next() == '/') {
      // single line comments
      while (peek() != '\n' && !is_at_end()) {
        advance();
      }
    } else if (c == '/' && peek_next() == '*') {
      // block comments
      advance(); // skip '/;
      advance(); // skip '*'
      while (!is_at_end()) {
        if (peek() == '*' && peek_next() == '/') {
          advance(); // consume *
          advance(); // consume /
          break;
        }
        if (is_at_end()) break;
        advance();
      }
    } else {
      break;
    }
  }
}

void Lexer::lex_string() {
  // Opening '"' was consumed by scan_token calling advance()
  std::string value;
  while (peek() != '"' && !is_at_end()) {
    char c = peek();
    if (c == '\\') {
      // handle escaped characters
      advance();
      if (is_at_end()) {
        m_logger.report(FatalError("Unterminated string escape sequence."));
        break;
      }
      char escaped_char = advance();
      switch (escaped_char) {
        case 'n': value += '\n'; break;
        case 't': value += '\t'; break;
        case '"': value += '"'; break;
        case '\\': value += '\\'; break;
        default:
          m_logger.report(Warning("Unterminated string escape sequence: \\" +
                                  std::string(1, escaped_char)));
          value += escaped_char;
          break;
      }
    } else {
      value += advance();
    }
  }

  if (is_at_end() || peek() != '"') {
    m_logger.report(FatalError("Unterminated string."));
    add_token(TokenType::UNKNOWN);
    return;
  }
  advance(); // Consume closing "
  add_token(TokenType::STRING_LITERAL, value);
}

void Lexer::lex_number() {
  std::string num_str;
  bool is_float = false;

  while (isdigit(peek())) {
    num_str += advance();
  }

  if (peek() == '.' && isdigit(peek_next())) {
    is_float = true;
    num_str += advance();
    while (isdigit(peek())) {
      num_str += advance();
    }
  }

  if (is_float) {
    try {
      add_token(TokenType::FLOAT_LITERAL, std::stod(num_str));
    } catch (const std::out_of_range&) {
      m_logger.report(FatalError("Float literal out of range: " + num_str));
      add_token(TokenType::UNKNOWN);
    }
  } else {
    try {
      add_token(TokenType::INT_LITERAL, std::stoull(num_str));
    } catch (const std::out_of_range&) {
      m_logger.report(FatalError("Integer literal out of range: " + num_str));
      add_token(TokenType::UNKNOWN);
    }
  }
}

void Lexer::lex_identifier_or_keyword() {
  // first char is already checked to be isalpha or '_', but not consumed
  std::string text;
  text += advance(); // consume the first character
  while (isalnum(peek()) || peek() == '_') {
    text += advance();
  }

  std::map<std::string, TokenType>::const_iterator i = s_keyword_map.find(text);
  if (i != s_keyword_map.end()) {
    add_token(i->second);
  } else {
    add_token(TokenType::IDENTIFIER);
  }
}

void Lexer::scan_token() {
  char c = peek();

  if (isalpha(c) || c == '_') {
    lex_identifier_or_keyword();
    return;
  }

  if (isdigit(c)) {
    lex_number();
    return;
  }

  // consume the character for single/multi-char tokens
  advance();

  switch (c) {
    case '(': add_token(TokenType::LPAREN); break;
    case ')': add_token(TokenType::RPAREN); break;
    case '{': add_token(TokenType::LBRACE); break;
    case '}': add_token(TokenType::RBRACE); break;
    case '[': add_token(TokenType::LBRACK); break;
    case ']': add_token(TokenType::RBRACK); break;
    case ',': add_token(TokenType::COMMA); break;
    case '.': add_token(TokenType::DOT); break;
    case ';': add_token(TokenType::SEMICOLON); break;
    case '+': add_token(TokenType::PLUS); break;
    case '*': add_token(TokenType::STAR); break;
    case '/': add_token(TokenType::SLASH); break;
    case '%': add_token(TokenType::MODULO); break;
    case '&': add_token(TokenType::AMPERSAND); break;

    case ':':
      add_token(match('=') ? TokenType::WALRUS : TokenType::COLON);
      break;
    case '=':
      add_token(match('=') ? TokenType::EQUAL_EQUAL : TokenType::EQUAL);
      break;
    case '!':
      add_token(match('=') ? TokenType::BANG_EQUAL : TokenType::BANG);
      break;
    case '<':
      add_token(match('=') ? TokenType::LESS_EQUAL : TokenType::LANGLE);
      break;
    case '>':
      add_token(match('=') ? TokenType::GREATER_EQUAL : TokenType::RANGLE);
      break;
    case '-':
      add_token(match('>') ? TokenType::ARROW : TokenType::MINUS);
      break;

    case '"': lex_string(); break;

    default:
      m_logger.report(FatalError("Unexpected character: " + std::string(1, c)));
      add_token(TokenType::UNKNOWN);
      break;
  }
}
