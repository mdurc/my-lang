#ifndef PARSER_PARSER_H
#define PARSER_PARSER_H

#include "../lexer/token.h"
#include "../logging/logger.h"
#include "ast.h"
#include "symtab.h"

class Parser {
public:
  Parser(SymTab* symtab, std::vector<Token> tokens);

private:
  Logger m_logger;
  bool m_panic_mode;

  SymTab* m_symtab;

  std::vector<Token> m_tokens;
  size_t m_pos;

  std::unique_ptr<ProgramNode> root;

  // Helpers
  AstNode* parse_chain(/* func pointer */);
  bool is_LValue();

  AstNode* parse_program();

  // Structs
  AstNode* parse_struct_decl();
  AstNode* parse_struct_member();
  AstNode* parse_struct_field();

  // Functions
  AstNode* parse_function_decl();
  AstNode* parse_function_param();
  AstNode* parse_function_return_type();

  // Statements
  AstNode* parse_statement();
  AstNode* parse_var_decl();
  AstNode* parse_assignment();
  AstNode* parse_if_stmt();
  AstNode* parse_for_stmt();
  AstNode* parse_while_stmt();
  AstNode* parse_switch_stmt();
  AstNode* parse_expression();
  AstNode* parse_return_stmt();
  AstNode* parse_break_stmt();
  AstNode* parse_continue_stmt();
  AstNode* parse_free_stmt();
  AstNode* parse_error_stmt();
  AstNode* parse_block();
  AstNode* parse_asm_block();

  // Expression Hierarchy
  AstNode* parse_logic_or();
  AstNode* parse_logic_and();
  AstNode* parse_equality();
  AstNode* parse_relational();
  AstNode* parse_additive();
  AstNode* parse_multiplicative();
  AstNode* parse_unary();
  AstNode* parse_postfix();
  AstNode* parse_postfix_suffix();

  AstNode* parse_args();
  AstNode* parse_type();

  // Primary Expressions
  AstNode* parse_primary();
  AstNode* parse_primitive_literal();
  AstNode* parse_struct_literal();
  AstNode* parse_new_expr();
};

#endif // PARSER_PARSER_H
