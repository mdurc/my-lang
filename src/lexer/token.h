#ifndef LEXER_TOKEN_H
#define LEXER_TOKEN_H

#include <cstdint>
#include <optional>
#include <ostream>
#include <string>
#include <variant>
#include <vector>

#include "span.h"

enum class TokenType {
  // Keywords:
  FUNC,     // func
  IF,       // if
  ELSE,     // else
  FOR,      // for
  WHILE,    // while
  RETURN,   // return
  RETURNS,  // returns
  BREAK,    // break
  CONTINUE, // continue
  TRUE,     // true
  FALSE,    // false
  NULL_,    // null
  AND,      // and
  OR,       // or
  STRUCT,   // struct
  SWITCH,   // switch
  CASE,     // case
  DEFAULT,  // default
  IMM,      // imm
  MUT,      // mut
  TAKE,     // take
  GIVE,     // give
  PTR,      // ptr
  NEW,      // new
  FREE,     // free
  ASM,      // asm
  ERROR_KW, // error
  EXIT_KW,  // exit
  READ,     // read
  PRINT,    // print

  IDENTIFIER,

  // Primitive type
  U0,     // u0
  U8,     // u8
  U16,    // u16
  U32,    // u32
  U64,    // u64
  I8,     // i8
  I16,    // i16
  I32,    // i32
  I64,    // i64
  F64,    // f64
  BOOL,   // bool
  STRING, // string

  // Literals
  INT_LITERAL,
  FLOAT_LITERAL,
  STRING_LITERAL,

  // Syntax
  LPAREN,    // (
  RPAREN,    // )
  LBRACE,    // {
  RBRACE,    // }
  LBRACK,    // [
  RBRACK,    // ]
  LANGLE,    // <
  RANGLE,    // >
  COMMA,     // ,
  COLON,     // :
  SEMICOLON, // ;
  DOT,       // .

  // Operators
  BANG,      // !
  PLUS,      // +
  MINUS,     // -
  SLASH,     // /
  STAR,      // *
  EQUAL,     // =
  AMPERSAND, // &
  MODULO,    // %

  // Compound operators:
  WALRUS,        // :=
  EQUAL_EQUAL,   // ==
  BANG_EQUAL,    // !=
  LESS_EQUAL,    // <=
  GREATER_EQUAL, // >=
  ARROW,         // ->

  UNKNOWN,
};

bool is_basic_type(TokenType type);

std::string token_type_to_string(TokenType type);

class Token {
public:
  // default constructable variant
  using Lit = std::variant<std::monostate, std::uint64_t, std::string, double>;

  // Span and Value are both going to be passed as literals thus
  // we are going to use std::move
  Token(TokenType type, const std::string& lexeme, Span span,
        Lit value = std::monostate{});

  TokenType get_type() const { return m_type; }
  const Span& get_span() const { return m_span; }
  const std::string& get_lexeme() const { return m_lexeme; }

  std::uint64_t get_int_val() const;
  const std::string& get_string_val() const;
  double get_float_val() const;

  bool is_literal() const;

  friend std::ostream& operator<<(std::ostream& out, const Token& token);

private:
  TokenType m_type;
  std::string m_lexeme;
  Span m_span;
  Lit m_literal_value;
};

void print_tokens(const std::vector<Token>& toks, std::ostream& out);

#endif // LEXER_TOKEN_H
