#ifndef TOKEN_H
#define TOKEN_H

#include <cstdint>
#include <ostream>
#include <string>
#include <variant>

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

  UNKNOWN,
  EOF_ // End Of File
};

std::string token_type_to_string(TokenType type);

class Span {
public:
  size_t row;
  size_t start_col;
  size_t end_col; // the span is exclusive of the end col
  Span(size_t r, size_t sc, size_t ec) : row(r), start_col(sc), end_col(ec) {}

  inline size_t len() { return end_col - start_col; }
};

class Token {
public:
  TokenType m_type;
  std::string m_lexeme;
  Span m_span;

  // default constructable variant
  using LiteralValueVariant =
      std::variant<std::monostate, std::uint64_t, std::string, double>;
  LiteralValueVariant m_literal_value;

  Token(TokenType type, const std::string& lexeme, const Span& span,
        LiteralValueVariant value = std::monostate{});

  bool is_literal() const;
  friend std::ostream& operator<<(std::ostream& out, const Token& token);
};

#endif
