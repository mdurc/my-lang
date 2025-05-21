#ifndef LEXER_TOKEN_H
#define LEXER_TOKEN_H

#include <cstdint>
#include <ostream>
#include <string>
#include <variant>

#include "span.h"

enum class TokenType {
  // Keywords:
  FUNC,          // func
  IF,            // if
  ELSE,          // else
  FOR,           // for
  WHILE,         // while
  RETURN,        // return
  RETURNS,       // returns
  BREAK,         // break
  CONTINUE,      // continue
  TRUE,          // true
  FALSE,         // false
  NULL_,         // null
  AND,           // and
  OR,            // or
  STRUCT,        // struct
  SWITCH,        // switch
  CASE,          // case
  DEFAULT,       // default
  IMM,           // imm
  MUT,           // mut
  READ,          // read
  TAKE,          // take
  GIVE,          // give
  PTR,           // ptr
  NEW,           // new
  FREE,          // free
  ASM,           // asm
  ERROR_KEYWORD, // Error
  PRINT,         // print

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
  EOF_ // End Of File
};

std::string token_type_to_string(TokenType type);

class Token {
public:
  // default constructable variant
  using Lit = std::variant<std::monostate, std::uint64_t, std::string, double>;

  Token(TokenType type, const std::string& lexeme, const Span& span,
        Lit value = std::monostate{});

  bool is_literal() const;
  friend std::ostream& operator<<(std::ostream& out, const Token& token);

private:
  TokenType m_type;
  std::string m_lexeme;
  Span m_span;
  Lit m_literal_value;
};

#endif // LEXER_TOKEN_H
