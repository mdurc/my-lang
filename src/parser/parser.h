#ifndef PARSER_PARSER_H
#define PARSER_PARSER_H

#include "../lexer/token.h"
#include "../logging/logger.h"
#include "ast.h"
#include "symtab.h"

class Parser {
public:
  Parser() = default;

  std::vector<AstPtr> parse_program(SymTab* symtab, std::vector<Token> tokens);

private:
  Logger m_logger;

  SymTab* m_symtab;

  std::vector<Token> m_tokens;
  size_t m_pos;

  std::vector<AstPtr> m_root;

  const Token* current();
  const Token* advance();
  bool consume(TokenType type);
  bool match(TokenType type) const;
  bool peek_next(TokenType type) const;

  bool is_sync_point(const Token* tok) const;
  void synchronize();

  AstPtr parse_toplevel_declaration();

  // Structs
  AstPtr parse_struct_decl();
  std::variant<StructFieldPtr, FuncDeclPtr> parse_struct_member();
  StructFieldPtr parse_struct_field();

  // Functions
  FuncDeclPtr parse_function_decl();
  BorrowState parse_function_param_prefix();
  ParamPtr parse_function_param();
  std::pair<std::string, std::shared_ptr<Type>> parse_function_return_type();

  // Statements
  StmtPtr parse_statement();
  bool is_next_var_decl();
  StmtPtr parse_var_decl();
  StmtPtr parse_if_stmt();
  StmtPtr parse_for_stmt();
  StmtPtr parse_while_stmt();
  StmtPtr parse_switch_stmt();
  StmtPtr parse_print_stmt();
  ExprPtr parse_expression();
  StmtPtr parse_return_stmt();
  StmtPtr parse_break_stmt();
  StmtPtr parse_continue_stmt();
  StmtPtr parse_free_stmt();
  StmtPtr parse_error_stmt();
  StmtPtr parse_exit_stmt();
  BlockPtr parse_block(bool create_scope);
  StmtPtr parse_asm_block();

  // Expression Hierarchy
  ExprPtr parse_assignment();
  ExprPtr parse_logic_or();
  ExprPtr parse_logic_and();
  ExprPtr parse_equality();
  ExprPtr parse_relational();
  ExprPtr parse_additive();
  ExprPtr parse_multiplicative();
  ExprPtr parse_unary();
  ExprPtr parse_postfix();

  std::vector<ArgPtr> parse_args();
  std::shared_ptr<Type> parse_type();

  // Primary Expressions
  ExprPtr parse_primary();
  ExprPtr parse_primitive_literal();
  ExprPtr parse_struct_literal(std::shared_ptr<Type> struct_type);
  ExprPtr parse_new_expr();
};

#endif // PARSER_PARSER_H
