#include "parser.h"

#include <cassert>

#define _consume(_type)                                            \
  do {                                                             \
    if (!consume(_type)) throw std::runtime_error("Parser error"); \
  } while (0)

#define _AST(_type, ...) std::make_shared<_type>(__VA_ARGS__)

// == Movement Operations through Tokens ==
const Token* Parser::current() {
  if (m_pos < m_tokens.size()) {
    return &m_tokens[m_pos];
  }
  m_logger.report(
      Error("Attempting to access current position when out of bounds"));
  throw std::runtime_error("Parser error");
}

// return token at the m_pos in the parser and then increment pos
const Token* Parser::advance() {
  if (m_pos < m_tokens.size()) {
    return &m_tokens[m_pos++];
  }
  m_logger.report(Error(
      "Attempting to access current position and advance when out of bounds"));
  throw std::runtime_error("Parser error");
}

// consume an expected type character at current position and increment position
bool Parser::consume(TokenType type) {
  if (match(type)) {
    advance();
    return true;
  }
  const Token* curr = current();
  m_logger.report(ExpectedToken(curr->get_span(), type, curr->get_type()));
  return false;
}

// check if the type of the Token at m_pos of the parser equals type
bool Parser::match(TokenType type) const {
  return m_pos < m_tokens.size() && m_tokens[m_pos].get_type() == type;
}

bool Parser::peek_next(TokenType type) const {
  if (m_pos + 1 < m_tokens.size()) {
    return m_tokens[m_pos + 1].get_type() == type;
  }
  return false;
}

// == Panic Mode Recovery ==
bool Parser::is_sync_point(const Token* tok) const {
  if (!tok) return false;
  switch (tok->get_type()) {
    case TokenType::SEMICOLON:
    case TokenType::FUNC:
    case TokenType::IF:
    case TokenType::STRUCT:
    case TokenType::WHILE:
    case TokenType::FOR:
    case TokenType::RETURN:
    case TokenType::READ:
    case TokenType::PRINT:
    case TokenType::LBRACE: return true;
    default: return false;
  }
}

void Parser::synchronize() {
  while (m_pos < m_tokens.size()) {
    if (is_sync_point(current())) {
      if (current()->get_type() == TokenType::SEMICOLON) {
        advance();
      }
      return;
    }
    advance();
  }
}

std::vector<AstPtr> Parser::parse_program(SymTab* symtab,
                                          std::vector<Token> tokens) {
  m_logger.clear();
  m_symtab = symtab;
  m_tokens = std::move(tokens);
  m_pos = 0;
  m_root.clear();

  while (m_pos < m_tokens.size()) {
    try {
      AstPtr node = parse_toplevel_declaration();
      m_root.push_back(node);
    } catch (const std::runtime_error&) {
      synchronize();
    }
  }

  std::string diags = m_logger.get_diagnostic_str();
  if (m_logger.num_errors() > 0 || m_logger.num_warnings() > 0) {
    throw std::runtime_error(
        diags + "Parsing failed with " + std::to_string(m_logger.num_errors()) +
        " errors and " + std::to_string(m_logger.num_warnings()) +
        " warnings.");
  }

  return m_root;
}

AstPtr Parser::parse_toplevel_declaration() {
  if (match(TokenType::STRUCT)) {
    return parse_struct_decl();
  } else if (match(TokenType::FUNC)) {
    return parse_function_decl();
  } else {
    return parse_statement();
  }
}

// Structs
// <StructDecl> ::= 'struct' Identifier '{' <StructFields>? '}'
AstPtr Parser::parse_struct_decl() {
  const Token* struct_tok = current();
  _consume(TokenType::STRUCT);

  const Token* name_tok = current();
  _consume(TokenType::IDENTIFIER);

  // initially 0 byte type as a placeholder for when the size is resolved
  // in the type checker.
  Type struct_type(Type::Named(name_tok->get_lexeme()),
                   m_symtab->current_scope(), 0);

  // declare it and see if it already exists (in which an error should occur)
  std::shared_ptr<Type> sym_struct_type = m_symtab->declare_type(struct_type);
  if (!sym_struct_type) {
    m_logger.report(DuplicateDeclarationError(name_tok->get_span(),
                                              struct_type.to_string()));
    throw std::runtime_error("Parser error");
  }

  m_symtab->enter_new_scope(); // new scope for the contents

  _consume(TokenType::LBRACE);

  std::vector<StructFieldPtr> fields;
  if (!match(TokenType::RBRACE)) {
    do {
      fields.push_back(parse_struct_field());
    } while (match(TokenType::COMMA) && advance());
  }

  _consume(TokenType::RBRACE);

  m_symtab->exit_scope();

  std::shared_ptr<StructDeclNode> node =
      _AST(StructDeclNode, struct_tok, m_symtab->current_scope(),
           sym_struct_type, std::move(fields));
  sym_struct_type->set_struct_decl(node);
  return node;
}

// <StructField> ::= Identifier ':' <Type>
StructFieldPtr Parser::parse_struct_field() {
  const Token* name_tok = current();
  _consume(TokenType::IDENTIFIER);

  IdentPtr name_ident = _AST(IdentifierNode, name_tok,
                             m_symtab->current_scope(), name_tok->get_lexeme());
  _consume(TokenType::COLON);

  std::shared_ptr<Type> type = parse_type();

  // declare it as a variable within the current scope which is struct decl
  Variable field_var(name_tok->get_lexeme(), BorrowState::MutablyOwned, type,
                     m_symtab->current_scope());

  // check if the field is a duplicate to another field
  if (!m_symtab->declare_variable(std::move(field_var))) {
    m_logger.report(DuplicateDeclarationError(name_tok->get_span(),
                                              name_tok->get_lexeme()));
    throw std::runtime_error("Parser error");
  }

  return _AST(StructFieldNode, name_tok, m_symtab->current_scope(), name_ident,
              type);
}

// Functions
// <FunctionDecl> ::= 'func' Identifier '(' <Params>? ')' <ReturnType>?
// <Block>
FuncDeclPtr Parser::parse_function_decl() {
  const Token* func_tok = current();
  _consume(TokenType::FUNC);

  const Token* name_tok = current();
  _consume(TokenType::IDENTIFIER);

  IdentPtr name_ident = _AST(IdentifierNode, name_tok,
                             m_symtab->current_scope(), name_tok->get_lexeme());

  m_symtab->enter_new_scope();

  _consume(TokenType::LPAREN);

  std::vector<ParamPtr> params_vec;
  if (!match(TokenType::RPAREN)) {
    do {
      params_vec.push_back(parse_function_param());
    } while (match(TokenType::COMMA) && advance());
  }

  _consume(TokenType::RPAREN);

  std::pair<std::optional<std::string>, std::shared_ptr<Type>> return_t =
      std::make_pair(std::nullopt, nullptr);
  if (match(TokenType::RETURNS)) {
    return_t = parse_function_return_type();
  } else {
    return_t.second = m_symtab->get_primitive_type("u0");
  }
  assert(return_t.second != nullptr);

  BlockPtr body_block = parse_block(false);

  m_symtab->exit_scope();

  // Construct FunctionType for the symbol table declaration
  std::vector<std::shared_ptr<Type>> ft_params;
  for (const ParamPtr& ast_param : params_vec) {
    ft_params.push_back(ast_param->type);
  }

  // make the function type. Lets make this declared at the global scope.
  // This makes it so that even if we are declaring a function type within a
  // struct, it can still exist as a type outside of the struct (not possible
  // anymore). Note that this is the only case where a function type wouldn't be
  // at the global scope.
  Type ft(Type::Function(std::move(ft_params), return_t.second), 0);

  // see if it already exists in the table, if so we will use that symbol
  // Function types are unique in that they can be "re-defined" without an
  // error, as the identifier associated to the func is what matters (and the
  // method family i.e. the types).
  // This allows for overloading functions.
  std::shared_ptr<Type> func_type = m_symtab->lookup_type(ft);
  if (!func_type) {
    // it doesn't exist, so we can add it as a new func-type
    func_type = m_symtab->declare_type(ft);
  }

  // declare it as a var with the associated type (also at global scope)
  Variable func_var(name_tok->get_lexeme(), BorrowState::ImmutableOwned,
                    func_type, 0);
  if (!m_symtab->declare_variable(std::move(func_var))) {
    m_logger.report(DuplicateDeclarationError(name_tok->get_span(),
                                              name_tok->get_lexeme()));
    throw std::runtime_error("Parser error");
  }

  return _AST(FunctionDeclNode, func_tok, m_symtab->current_scope(), name_ident,
              std::move(params_vec), std::move(return_t.first), return_t.second,
              body_block, func_type);
}

BorrowState Parser::parse_function_param_prefix() {
  BorrowState modifier = BorrowState::ImmutablyBorrowed; // default
  if (match(TokenType::MUT)) {
    modifier = BorrowState::MutablyBorrowed;
    advance();
  } else if (match(TokenType::IMM)) {
    advance(); // imm borrowed is already the default
  } else if (match(TokenType::TAKE)) {
    advance();
    modifier = BorrowState::ImmutableOwned; // Take ==> imm owned by default
    if (match(TokenType::MUT)) {
      modifier = BorrowState::MutablyOwned;
      advance();
    } else if (match(TokenType::IMM)) {
      advance();
    }
  }
  return modifier;
}

// <Param> ::= <FunctionParamPrefix>? Identifier ':' <Type>
ParamPtr Parser::parse_function_param() {
  const Token* first_tok = current();

  BorrowState modifier = parse_function_param_prefix();

  const Token* name_tok = current();
  _consume(TokenType::IDENTIFIER);

  IdentPtr name_ident = _AST(IdentifierNode, name_tok,
                             m_symtab->current_scope(), name_tok->get_lexeme());

  _consume(TokenType::COLON);

  std::shared_ptr<Type> type_val = parse_type();

  // declare the parameter as a variable in current scope
  Variable param_var(name_tok->get_lexeme(), modifier, type_val,
                     m_symtab->current_scope());
  if (!m_symtab->declare_variable(std::move(param_var))) {
    m_logger.report(DuplicateDeclarationError(name_tok->get_span(),
                                              name_tok->get_lexeme()));
    throw std::runtime_error("Parser error");
  }

  return _AST(ParamNode, first_tok, m_symtab->current_scope(), modifier,
              name_ident, type_val);
}

// <ReturnType> ::= 'returns' '(' Identifier ':' <Type> ')'
std::pair<std::string, std::shared_ptr<Type>>
Parser::parse_function_return_type() {
  _consume(TokenType::RETURNS);

  _consume(TokenType::LPAREN);

  const Token* ident_tok = current();
  _consume(TokenType::IDENTIFIER);

  _consume(TokenType::COLON);

  std::shared_ptr<Type> type_val = parse_type();

  _consume(TokenType::RPAREN);

  // MutablyOwned is required
  Variable return_var(ident_tok->get_lexeme(), BorrowState::MutablyOwned,
                      type_val, m_symtab->current_scope(), true);
  if (!m_symtab->declare_variable(std::move(return_var))) {
    m_logger.report(DuplicateDeclarationError(ident_tok->get_span(),
                                              ident_tok->get_lexeme()));
    throw std::runtime_error("Parser error");
  }

  return std::make_pair(ident_tok->get_lexeme(), type_val);
}

bool Parser::is_next_var_decl() {
  if (match(TokenType::MUT) || match(TokenType::IMM)) {
    return true;
  } else if (match(TokenType::IDENTIFIER)) {
    // with either colon or walrus it will be a variable declaration
    if ((peek_next(TokenType::COLON) || peek_next(TokenType::WALRUS))) {
      return true;
    }
  }
  return false;
}

// Statements
StmtPtr Parser::parse_statement() {
  // Handle case where the identifier can be the start of a var declaration
  if (is_next_var_decl()) {
    return parse_var_decl();
  }

  TokenType type = current()->get_type();
  switch (type) {
    case TokenType::IF: return parse_if_stmt();
    case TokenType::FOR: return parse_for_stmt();
    case TokenType::WHILE: return parse_while_stmt();
    case TokenType::SWITCH: return parse_switch_stmt();
    case TokenType::READ: {
      const Token* read_tok = advance();
      ExprPtr expr = parse_expression(); // should be an identifier, verified by
                                         // type checker
      _consume(TokenType::SEMICOLON);
      return _AST(ReadStmtNode, read_tok, m_symtab->current_scope(), expr);
    }
    case TokenType::PRINT: return parse_print_stmt();
    case TokenType::LBRACE: return parse_block(true);
    case TokenType::RETURN: return parse_return_stmt();
    case TokenType::BREAK: return parse_break_stmt();
    case TokenType::CONTINUE: return parse_continue_stmt();
    case TokenType::FREE: return parse_free_stmt();
    case TokenType::ERROR_KW: return parse_error_stmt();
    case TokenType::EXIT_KW: return parse_exit_stmt();
    case TokenType::ASM: return parse_asm_block();
    default: {
      // <Expr> ';'
      const Token* expr_start_tok = current();
      ExprPtr expr = parse_expression();

      // return the expression stmt
      _consume(TokenType::SEMICOLON);
      return _AST(ExpressionStatementNode, expr_start_tok,
                  m_symtab->current_scope(), expr);
    }
  }
}

// <VarDecl> ::= <TypePrefix>? Identifier ( ( ':' <Type> ( '=' <Expr> )? ) | (
// ':=' <Expr> ) )
StmtPtr Parser::parse_var_decl() {
  const Token* start_tok = current();

  bool is_mutable = false; // imm by default
  if (match(TokenType::MUT)) {
    advance();
    is_mutable = true;
  } else if (match(TokenType::IMM)) {
    advance();
  }

  const Token* name_tok = current();
  _consume(TokenType::IDENTIFIER);
  IdentPtr var_name = _AST(IdentifierNode, name_tok, m_symtab->current_scope(),
                           name_tok->get_lexeme());

  std::shared_ptr<Type> type_val = nullptr;

  ExprPtr initializer = nullptr;

  if (match(TokenType::COLON)) { // ':' <Type> ( '=' <Expr> )?
    // explicit type declaration
    advance();
    type_val = parse_type();
    if (match(TokenType::EQUAL)) {
      advance();
      initializer = parse_expression();
    }
  } else if (match(TokenType::WALRUS)) { // ':=' <Expr>
    // inferred type declaration (type_val is nullptr)
    advance();
    initializer = parse_expression();
  } else {
    m_logger.report(
        ExpectedToken(current()->get_span(), ": | :=", current()->get_type()));
    throw std::runtime_error("Parser error");
  }

  _consume(TokenType::SEMICOLON);

  BorrowState bs =
      is_mutable ? BorrowState::MutablyOwned : BorrowState::ImmutableOwned;

  Variable var_sym(name_tok->get_lexeme(), bs, type_val,
                   m_symtab->current_scope());

  // check if it is a duplicate variable
  if (!m_symtab->declare_variable(std::move(var_sym))) {
    m_logger.report(DuplicateDeclarationError(name_tok->get_span(),
                                              name_tok->get_lexeme()));
    throw std::runtime_error("Parser error");
  }

  return _AST(VariableDeclNode, start_tok, m_symtab->current_scope(),
              is_mutable, var_name, type_val, initializer);
}

// <IfStmt> ::= 'if' '(' <Expr> ')' <Block> ( 'else' ( <Block> | <IfStmt> ) )?
StmtPtr Parser::parse_if_stmt() {
  const Token* if_tok = current();
  _consume(TokenType::IF);
  _consume(TokenType::LPAREN);

  ExprPtr condition = parse_expression();
  _consume(TokenType::RPAREN);

  BlockPtr then_branch = parse_block(true);

  BlockPtr else_branch = nullptr;
  if (match(TokenType::ELSE)) {
    advance();
    if (match(TokenType::IF)) {
      // else if block
      StmtPtr else_if_stmt = parse_if_stmt();
      std::vector<StmtPtr> else_if_stmts;
      else_if_stmts.push_back(else_if_stmt);
      else_branch =
          _AST(BlockNode, current(), m_symtab->current_scope(), else_if_stmts);
    } else {
      // else block
      else_branch = parse_block(true);
    }
  }
  return _AST(IfStmtNode, if_tok, m_symtab->current_scope(), condition,
              then_branch, else_branch);
}

// <ForStmt> ::= 'for' '(' <Expr>? ';' <Expr>? ';' <Expr>? ')' <Block>
StmtPtr Parser::parse_for_stmt() {
  const Token* for_tok = current();
  _consume(TokenType::FOR);

  m_symtab->enter_new_scope();

  _consume(TokenType::LPAREN);

  std::optional<std::variant<ExprPtr, StmtPtr>> initializer = std::nullopt;
  if (!match(TokenType::SEMICOLON)) {
    if (is_next_var_decl()) {
      initializer = parse_var_decl();
    } else {
      initializer = parse_expression();
      _consume(TokenType::SEMICOLON);
    }
  }

  ExprPtr condition = nullptr;
  if (!match(TokenType::SEMICOLON)) {
    condition = parse_expression();
  }
  _consume(TokenType::SEMICOLON);

  ExprPtr iteration = nullptr;
  if (!match(TokenType::RPAREN)) {
    iteration = parse_expression();
  }
  _consume(TokenType::RPAREN);

  BlockPtr body = parse_block(true);

  m_symtab->exit_scope();

  return _AST(ForStmtNode, for_tok, m_symtab->current_scope(),
              std::move(initializer), condition, iteration, body);
}

// <WhileStmt> ::= 'while' '(' <Expr> ')' <Block>
StmtPtr Parser::parse_while_stmt() {
  const Token* while_tok = current();
  _consume(TokenType::WHILE);

  _consume(TokenType::LPAREN);

  ExprPtr condition = parse_expression();
  _consume(TokenType::RPAREN);

  BlockPtr body = parse_block(true);

  return _AST(WhileStmtNode, while_tok, m_symtab->current_scope(), condition,
              body);
}

// <SwitchStmt> ::= 'switch' '(' <Expr> ')' '{' <Case>* '}'
StmtPtr Parser::parse_switch_stmt() {
  const Token* switch_tok = current();
  _consume(TokenType::SWITCH);

  _consume(TokenType::LPAREN);

  ExprPtr expression = parse_expression();
  _consume(TokenType::RPAREN);

  _consume(TokenType::LBRACE);

  std::vector<CasePtr> cases;
  while (!match(TokenType::RBRACE)) {
    // <Case> ::= 'case' <Expr> ':' <Block> | 'default' ':' <Block>
    const Token* case_tok = current();
    ExprPtr value = nullptr;
    if (match(TokenType::CASE)) {
      advance();
      value = parse_expression();
    } else if (match(TokenType::DEFAULT)) {
      advance(); // value remains nullptr for default
    } else {
      m_logger.report(ExpectedToken(current()->get_span(), "case | default",
                                    current()->get_type()));
      throw std::runtime_error("Parser error");
    }

    _consume(TokenType::COLON);

    BlockPtr body = parse_block(true); // each case will have its own scope

    cases.push_back(
        _AST(CaseNode, case_tok, m_symtab->current_scope(), body, value));
  }

  _consume(TokenType::RBRACE);

  return _AST(SwitchStmtNode, switch_tok, m_symtab->current_scope(), expression,
              std::move(cases));
}

// <PrintStmt> ::= 'print'  <Expr> ( ',' <Expr> )*
StmtPtr Parser::parse_print_stmt() {
  const Token* print_tok = advance();

  std::vector<ExprPtr> exprs;
  do {
    exprs.push_back(parse_expression());
  } while (match(TokenType::COMMA) && advance());
  _consume(TokenType::SEMICOLON);
  return _AST(PrintStmtNode, print_tok, m_symtab->current_scope(), exprs);
}

// == Expression Parsing ==
ExprPtr Parser::parse_expression() {
  // start with lowest precedence in recursive descent
  return parse_assignment();
}

// <ReturnStmt> ::= 'return' <Expr>?
StmtPtr Parser::parse_return_stmt() {
  const Token* ret_tok = current();
  _consume(TokenType::RETURN);

  ExprPtr value = nullptr;
  if (!match(TokenType::SEMICOLON)) {
    value = parse_expression();
  }
  _consume(TokenType::SEMICOLON);

  return _AST(ReturnStmtNode, ret_tok, m_symtab->current_scope(), value);
}

// <BreakStmt> ::= 'break'
StmtPtr Parser::parse_break_stmt() {
  const Token* break_tok = current();
  _consume(TokenType::BREAK);
  _consume(TokenType::SEMICOLON);
  return _AST(BreakStmtNode, break_tok, m_symtab->current_scope());
}

// <ContinueStmt> ::= 'continue'
StmtPtr Parser::parse_continue_stmt() {
  const Token* cont_tok = current();
  _consume(TokenType::CONTINUE);
  _consume(TokenType::SEMICOLON);
  return _AST(ContinueStmtNode, cont_tok, m_symtab->current_scope());
}

// <FreeStmt> ::= 'free' '[]'? <Expr>
StmtPtr Parser::parse_free_stmt() {
  const Token* free_tok = current();
  _consume(TokenType::FREE);

  bool is_array = false;
  if (match(TokenType::LBRACK)) {
    advance();
    _consume(TokenType::RBRACK);

    is_array = true;
  }

  ExprPtr expr = parse_expression();
  _consume(TokenType::SEMICOLON);

  return _AST(FreeStmtNode, free_tok, m_symtab->current_scope(), is_array,
              expr);
}

// <ErrorStmt> ::= 'Error' String
StmtPtr Parser::parse_error_stmt() {
  const Token* error_tok = current();
  _consume(TokenType::ERROR_KW);

  const Token* str_tok = current();
  _consume(TokenType::STRING_LITERAL);

  if (!str_tok->is_literal()) {
    m_logger.report(Error(
        str_tok->get_span(),
        "Internal Error: Expected string literal token to have string data"));
    throw std::runtime_error("Parser error");
  }
  const std::string& msg = str_tok->get_string_val();

  _consume(TokenType::SEMICOLON);

  return _AST(ErrorStmtNode, error_tok, m_symtab->current_scope(), msg);
}

// <ExitStmt> ::= 'exit' (1 | 0)
StmtPtr Parser::parse_exit_stmt() {
  const Token* exit_tok = current();
  _consume(TokenType::EXIT_KW);

  const Token* str_tok = current();
  _consume(TokenType::INT_LITERAL);
  _consume(TokenType::SEMICOLON);

  assert(str_tok->is_literal());

  return _AST(ExitStmtNode, exit_tok, m_symtab->current_scope(),
              str_tok->get_int_val());
}

// <Block> ::= '{' <Stmt>* '}'
BlockPtr Parser::parse_block(bool create_scope) {
  const Token* lbrace_tok = current();

  if (create_scope) {
    m_symtab->enter_new_scope();
  }

  _consume(TokenType::LBRACE);
  std::vector<StmtPtr> statements_vec;
  while (!match(TokenType::RBRACE) && m_pos < m_tokens.size()) {
    statements_vec.push_back(parse_statement());
  }
  _consume(TokenType::RBRACE);

  if (create_scope) {
    m_symtab->exit_scope();
  }

  return _AST(BlockNode, lbrace_tok, m_symtab->current_scope(),
              std::move(statements_vec));
}

// <Stmt> ::= ... | 'asm' <Block> ';'
StmtPtr Parser::parse_asm_block() {
  const Token* asm_tok = current();
  _consume(TokenType::ASM);

  Span prev_span = current()->get_span();
  _consume(TokenType::LBRACE);

  std::string asm_body_str; // raw string for now

  bool is_first = true;
  int brace_level = 1;
  while (m_pos < m_tokens.size()) {
    const Token* current_tok = current();
    TokenType current_type = current_tok->get_type();

    if (current_type == TokenType::LBRACE) {
      brace_level++;
    } else if (current_type == TokenType::RBRACE) {
      brace_level--;
    }

    if (brace_level == 0) {
      // final closing RBRACE
      break;
    }

    const Span& curr_span = current_tok->get_span();

    // Add newlines if current token is on a new line relative to the prev
    if (!is_first) {
      if (curr_span.row > prev_span.row) {
        for (size_t i = 0; i < curr_span.row - prev_span.row; ++i) {
          asm_body_str += '\n';
        }
      } else {
        // curr token is on the same line, separate by space
        asm_body_str += ' ';
      }
    }
    is_first = false;

    asm_body_str += current_tok->get_lexeme();
    prev_span = curr_span;
    advance();
  }

  if (brace_level != 0 || m_pos >= m_tokens.size()) {
    m_logger.report(ExpectedToken(current()->get_span(), "}", "<none>"));
    throw std::runtime_error("Parser error");
  }

  _consume(TokenType::RBRACE);
  _consume(TokenType::SEMICOLON);

  return _AST(AsmBlockNode, asm_tok, m_symtab->current_scope(), asm_body_str);
}

// Expression Hierarchy
ExprPtr Parser::parse_assignment() {
  ExprPtr left = parse_logic_or();

  if (match(TokenType::EQUAL)) {
    const Token* tok = advance();
    ExprPtr right = parse_assignment();
    return _AST(AssignmentNode, tok, m_symtab->current_scope(), left, right);
  }

  return left;
}

// <LogicalOrExpr> ::= <LogicalAndExpr> ( 'or' <LogicalAndExpr> )*
ExprPtr Parser::parse_logic_or() {
  ExprPtr expr = parse_logic_and();
  while (match(TokenType::OR)) {
    const Token* op_tok = advance();
    ExprPtr right = parse_logic_and();
    expr = _AST(BinaryOpExprNode, op_tok, m_symtab->current_scope(),
                BinOperator::LogicalOr, expr, right);
  }
  return expr;
}

// <LogicalAndExpr> ::= <EqualityExpr> ( 'and' <EqualityExpr> )*
ExprPtr Parser::parse_logic_and() {
  ExprPtr expr = parse_equality();
  while (match(TokenType::AND)) {
    const Token* op_tok = advance();
    ExprPtr right = parse_equality();
    expr = _AST(BinaryOpExprNode, op_tok, m_symtab->current_scope(),
                BinOperator::LogicalAnd, expr, right);
  }
  return expr;
}

// <EqualityExpr> ::= <RelationalExpr> ( ( '==' | '!=' ) <RelationalExpr> )*
ExprPtr Parser::parse_equality() {
  ExprPtr expr = parse_relational();
  while (match(TokenType::EQUAL_EQUAL) || match(TokenType::BANG_EQUAL)) {
    const Token* op_tok = advance();
    BinOperator op = (op_tok->get_type() == TokenType::EQUAL_EQUAL)
                         ? BinOperator::Equal
                         : BinOperator::NotEqual;
    ExprPtr right = parse_relational();
    expr = _AST(BinaryOpExprNode, op_tok, m_symtab->current_scope(), op, expr,
                right);
  }
  return expr;
}

// <RelationalExpr> ::= <AdditiveExpr> ( ( '<' | '>' | '<=' | '>=' )
// <AdditiveExpr> )*
ExprPtr Parser::parse_relational() {
  ExprPtr expr = parse_additive();
  while (match(TokenType::LANGLE) || match(TokenType::RANGLE) ||
         match(TokenType::LESS_EQUAL) || match(TokenType::GREATER_EQUAL)) {
    const Token* op_tok = advance();

    BinOperator op;
    switch (op_tok->get_type()) {
      case TokenType::LANGLE: op = BinOperator::LessThan; break;
      case TokenType::RANGLE: op = BinOperator::GreaterThan; break;
      case TokenType::LESS_EQUAL: op = BinOperator::LessEqual; break;
      case TokenType::GREATER_EQUAL: op = BinOperator::GreaterEqual; break;
      default: {
        m_logger.report(
            Error(op_tok->get_span(), "Unknown binary operator type"));
        throw std::runtime_error("Parser error");
      }
    }

    ExprPtr right = parse_additive();
    expr = _AST(BinaryOpExprNode, op_tok, m_symtab->current_scope(), op, expr,
                right);
  }
  return expr;
}

// <AdditiveExpr> ::= <MultiplicativeExpr> ( ( '+' | '-') <MultiplicativeExpr>
// )*
ExprPtr Parser::parse_additive() {
  ExprPtr expr = parse_multiplicative();
  while (match(TokenType::PLUS) || match(TokenType::MINUS)) {
    const Token* op_tok = advance();
    BinOperator op = (op_tok->get_type() == TokenType::PLUS)
                         ? BinOperator::Plus
                         : BinOperator::Minus;

    ExprPtr right = parse_multiplicative();
    expr = _AST(BinaryOpExprNode, op_tok, m_symtab->current_scope(), op, expr,
                right);
  }
  return expr;
}

// <MultiplicativeExpr> ::= <UnaryExpr> ( ( '*' | '/' | '%' ) <UnaryExpr> )*
ExprPtr Parser::parse_multiplicative() {
  ExprPtr expr = parse_unary();
  while (match(TokenType::STAR) || match(TokenType::SLASH) ||
         match(TokenType::MODULO)) {
    const Token* op_tok = advance();

    BinOperator op;
    switch (op_tok->get_type()) {
      case TokenType::STAR: op = BinOperator::Multiply; break;
      case TokenType::SLASH: op = BinOperator::Divide; break;
      case TokenType::MODULO: op = BinOperator::Modulo; break;
      default: {
        m_logger.report(Error(op_tok->get_span(),
                              "Unknown multiplicative binary operator type"));
        throw std::runtime_error("Parser error");
      }
    }

    ExprPtr right = parse_unary();
    expr = _AST(BinaryOpExprNode, op_tok, m_symtab->current_scope(), op, expr,
                right);
  }
  return expr;
}

// <UnaryExpr> ::= ( '&' <TypePrefix>? | '*' | '!' ) <UnaryExpr> | <PostfixExpr>
ExprPtr Parser::parse_unary() {
  const Token* op_tok = current();

  TokenType type = current()->get_type();
  switch (type) {
    case TokenType::AMPERSAND: {
      advance();
      UnaryOperator op = UnaryOperator::AddressOf;
      if (match(TokenType::MUT)) {
        advance();
        op = UnaryOperator::AddressOfMut;
      } else if (match(TokenType::IMM)) {
        advance(); // still just AddressOf which implies immutable ref
      }
      // recur in order to find joined unary chains
      ExprPtr operand = parse_unary();
      return _AST(UnaryExprNode, op_tok, m_symtab->current_scope(), op,
                  operand);
    }
    case TokenType::STAR: {
      // dereference operation
      advance();
      ExprPtr operand = parse_unary();
      return _AST(UnaryExprNode, op_tok, m_symtab->current_scope(),
                  UnaryOperator::Dereference, operand);
    }
    case TokenType::BANG: {
      advance();
      ExprPtr operand = parse_unary();
      return _AST(UnaryExprNode, op_tok, m_symtab->current_scope(),
                  UnaryOperator::LogicalNot, operand);
    }
    case TokenType::MINUS: {
      advance();
      ExprPtr operand = parse_unary();
      return _AST(UnaryExprNode, op_tok, m_symtab->current_scope(),
                  UnaryOperator::Negate, operand);
    }
    default: break;
  }
  return parse_postfix();
}

// <PostfixExpr> ::= <PrimaryExpr> ( <PostfixSuffix> )*
ExprPtr Parser::parse_postfix() {
  ExprPtr expr = parse_primary();

  while (true) {
    const Token* op_tok = current();

    TokenType type = current()->get_type();
    switch (type) {
      case TokenType::LBRACK: {
        // Array Index: '[' <Expr> ']'
        advance();
        ExprPtr index_expr = parse_expression();

        _consume(TokenType::RBRACK);
        expr = _AST(ArrayIndexNode, op_tok, m_symtab->current_scope(), expr,
                    index_expr);
        break;
      }
      case TokenType::LPAREN: {
        // function call
        advance();
        std::vector<ArgPtr> args_vec = parse_args();
        _consume(TokenType::RPAREN);

        expr = _AST(FunctionCallNode, op_tok, m_symtab->current_scope(), expr,
                    std::move(args_vec));
        break;
      }
      case TokenType::DOT: {
        // field access via dot
        advance();
        const Token* field_name_tok = current();
        _consume(TokenType::IDENTIFIER);

        IdentPtr field_ident =
            _AST(IdentifierNode, field_name_tok, m_symtab->current_scope(),
                 field_name_tok->get_lexeme());
        expr = _AST(FieldAccessNode, op_tok, m_symtab->current_scope(), expr,
                    field_ident);
        break;
      }
      default: return expr;
    }
  }
  return expr;
}

// <Args> ::= <Arg> ( ',' <Arg> )*
std::vector<ArgPtr> Parser::parse_args() {
  std::vector<ArgPtr> args_vec;
  if (match(TokenType::RPAREN)) {
    return args_vec;
  }

  do {
    // <Arg> ::= 'give'? <Expr>
    const Token* arg_tok = current();
    bool is_give = false;
    if (match(TokenType::GIVE)) {
      advance();
      is_give = true;
    }

    ExprPtr arg_expr = parse_expression();
    args_vec.push_back(_AST(ArgumentNode, arg_tok, m_symtab->current_scope(),
                            is_give, arg_expr));
  } while (match(TokenType::COMMA) && advance());

  return args_vec;
}

// <Type> ::= <BasicType> | <StructType> | <PointerType> | <FunctionType>
std::shared_ptr<Type> Parser::parse_type() {
  const Token* type_start_tok = current();
  if (is_basic_type(type_start_tok->get_type())) {
    // Basic types
    advance();
    std::shared_ptr<Type> t =
        m_symtab->get_primitive_type(type_start_tok->get_lexeme());
    assert(t != nullptr);
    return t;
  }
  if (match(TokenType::IDENTIFIER)) {
    // Struct type
    advance();
    std::shared_ptr<Type> t =
        m_symtab->lookup_type(Type(Type::Named(type_start_tok->get_lexeme()),
                                   m_symtab->current_scope(), -1));
    if (t == nullptr) {
      m_logger.report(TypeNotFoundError(type_start_tok->get_span(),
                                        type_start_tok->get_lexeme()));
      throw std::runtime_error("Parser error");
    }
    return t;
  } else if (match(TokenType::PTR)) {
    // Pointer type
    // <PointerType> ::= 'ptr' '<' <TypePrefix>? <Type> '>'
    advance();
    _consume(TokenType::LANGLE);

    bool is_mutable = false; // imm by default
    if (match(TokenType::MUT)) {
      advance();
      is_mutable = true;
    } else if (match(TokenType::IMM)) {
      advance();
    }

    std::shared_ptr<Type> pointee = parse_type();
    _consume(TokenType::RANGLE);

    // we do not check the symbol table for this because ptrs of any valid
    // pointee types are allowed. We will add it to the symbol table though.

    Type search_type(Type::Pointer(is_mutable, pointee),
                     m_symtab->current_scope());

    std::shared_ptr<Type> ptr_type = m_symtab->lookup_type(search_type);
    if (!ptr_type) {
      // it doesn't exist, so we can add
      ptr_type = m_symtab->declare_type(search_type);
    }

    return ptr_type;
  } else if (match(TokenType::FUNC)) {
    // <FunctionType> ::= 'func' '(' (<Type> ( ',' <Type> )*)? ')' '->' <Type>
    advance();
    _consume(TokenType::LPAREN);

    std::vector<std::shared_ptr<Type>> param_types;
    if (!match(TokenType::RPAREN)) {
      do {
        param_types.push_back(parse_type());
      } while (match(TokenType::COMMA) && advance());
    }
    _consume(TokenType::RPAREN);

    _consume(TokenType::ARROW);

    std::shared_ptr<Type> ret_type = parse_type();

    // functions are declared at scope 0
    Type func_type(Type::Function(std::move(param_types), ret_type), 0);
    std::shared_ptr<Type> t = m_symtab->lookup_type(func_type);
    if (t == nullptr) {
      m_logger.report(
          TypeNotFoundError(type_start_tok->get_span(), func_type.to_string()));
      throw std::runtime_error("Parser error");
    }
    return t;
  }

  m_logger.report(ExpectedToken(type_start_tok->get_span(), "<type>",
                                type_start_tok->get_type()));
  throw std::runtime_error("Parser error");
}

// Primary Expressions
ExprPtr Parser::parse_primary() {
  const Token* tok = current();

  if (match(TokenType::INT_LITERAL) || match(TokenType::FLOAT_LITERAL) ||
      match(TokenType::STRING_LITERAL) || match(TokenType::TRUE) ||
      match(TokenType::FALSE) || match(TokenType::NULL_)) {
    return parse_primitive_literal();
  } else if (match(TokenType::IDENTIFIER)) {
    // could be variable identifier or struct literal

    // if it exists as a type, then it is a struct because that is the only
    // other named type that can exist besides primatives
    std::shared_ptr<Type> struct_type = m_symtab->lookup_type(
        Type(Type::Named(tok->get_lexeme()), m_symtab->current_scope(), -1));
    if (struct_type) {
      return parse_struct_literal(struct_type);
    }

    // otherwise it is an identifier
    advance();
    return _AST(IdentifierNode, tok, m_symtab->current_scope(),
                tok->get_lexeme());
  } else if (match(TokenType::LPAREN)) {
    // Grouped Expression: '(' <Expr> ')'
    advance();
    ExprPtr grouped_expr = parse_expression();
    _consume(TokenType::RPAREN);

    return _AST(GroupedExprNode, tok, m_symtab->current_scope(), grouped_expr);
  } else if (match(TokenType::NEW)) {
    return parse_new_expr();
  }

  // Otherwise it is invalid
  m_logger.report(ExpectedToken(tok->get_span(), "<primary>", tok->get_type()));
  throw std::runtime_error("Parser error");
}

// <PrimitiveLiteral> ::= Integer | Float | String | Bool | 'null'
ExprPtr Parser::parse_primitive_literal() {
  const Token* tok = current();

  TokenType type = current()->get_type();
  advance();
  switch (type) {
    case TokenType::INT_LITERAL:
      return _AST(IntegerLiteralNode, tok, m_symtab->current_scope(),
                  tok->get_int_val());
    case TokenType::FLOAT_LITERAL:
      return _AST(FloatLiteralNode, tok, m_symtab->current_scope(),
                  tok->get_float_val());
    case TokenType::STRING_LITERAL:
      return _AST(StringLiteralNode, tok, m_symtab->current_scope(),
                  tok->get_string_val());
    case TokenType::TRUE:
      return _AST(BoolLiteralNode, tok, m_symtab->current_scope(), true);
    case TokenType::FALSE:
      return _AST(BoolLiteralNode, tok, m_symtab->current_scope(), false);
    case TokenType::NULL_:
      return _AST(NullLiteralNode, tok, m_symtab->current_scope());
    default: break;
  }

  m_logger.report(ExpectedToken(tok->get_span(), "<literal>", tok->get_type()));
  throw std::runtime_error("Parser error");
}

// <StructLiteral> ::= Identifier '(' ( Identifier '=' <Expr> ( ',' Identifier
// '=' <Expr> )* )? ')'
ExprPtr Parser::parse_struct_literal(std::shared_ptr<Type> struct_type) {
  const Token* type_name_tok = current(); // this is the Type that is passed
  _consume(TokenType::IDENTIFIER);

  _consume(TokenType::LPAREN);

  std::vector<StructFieldInitPtr> initializers_vec;
  if (!match(TokenType::RPAREN)) {
    do {
      const Token* field_name_tok = current();
      _consume(TokenType::IDENTIFIER);

      IdentPtr field_ident =
          _AST(IdentifierNode, field_name_tok, m_symtab->current_scope(),
               field_name_tok->get_lexeme());

      _consume(TokenType::EQUAL);

      ExprPtr value_expr = parse_expression();

      initializers_vec.push_back(_AST(StructFieldInitializerNode,
                                      field_name_tok, m_symtab->current_scope(),
                                      field_ident, value_expr));
    } while (match(TokenType::COMMA) && advance());
  }

  _consume(TokenType::RPAREN);

  return _AST(StructLiteralNode, type_name_tok, m_symtab->current_scope(),
              struct_type, std::move(initializers_vec));
}

// <NewExpr> ::= 'new' '<' <TypePrefix>? <Type> '>' ( '[' <Expr> ']' | '('
// <Expr>? ')' )

// <NewExpr> ::= 'new' '<'<TypePrefix>? <Type>'>'
// ( '['<Expr>']' | '('(<StructLiteral> | <Expr>)?')' )

ExprPtr Parser::parse_new_expr() {
  const Token* new_tok = current();
  _consume(TokenType::NEW);

  _consume(TokenType::LANGLE);

  bool is_memory_mutable = false; // imm by default
  if (match(TokenType::MUT)) {
    advance();
    is_memory_mutable = true;
  } else if (match(TokenType::IMM)) {
    advance();
  }

  std::shared_ptr<Type> allocated_type = parse_type();

  _consume(TokenType::RANGLE);

  bool is_array = false;
  ExprPtr specifier = nullptr;
  if (match(TokenType::LBRACK)) {
    // Array allocation: '[' <Expr> ']'
    is_array = true;
    advance();
    specifier = parse_expression();
    _consume(TokenType::RBRACK);
  } else if (match(TokenType::LPAREN)) {
    // Constructor call: '(' <Expr>? ')'
    advance();
    if (!match(TokenType::RPAREN)) {
      // Check if the current token is the start of a declared Struct Type
      std::shared_ptr<Type> struct_type = m_symtab->lookup_type(Type(
          Type::Named(current()->get_lexeme()), m_symtab->current_scope(), -1));
      if (struct_type) {
        // This must be the start of a struct literal
        specifier = parse_struct_literal(struct_type);
      } else {
        // It is a standard constructor type (primitives, literals, and
        // expressions that resolve to those)
        specifier = parse_expression();
      }
    }
    _consume(TokenType::RPAREN);
  } else {
    m_logger.report(ExpectedToken(current()->get_span(), "[ | < | (",
                                  current()->get_type()));
    throw std::runtime_error("Parser error");
  }
  return _AST(NewExprNode, new_tok, m_symtab->current_scope(),
              is_memory_mutable, is_array, allocated_type, specifier);
}
