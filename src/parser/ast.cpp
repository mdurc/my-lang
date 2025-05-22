#include "ast.h"

ExprPtr Ast::make_int_lit(Token* tok, size_t sc, uint64_t val) {
  return std::make_unique<IntegerLiteralNode>(tok, sc, val);
}

ExprPtr Ast::make_float_lit(Token* tok, size_t sc, double val) {
  return std::make_unique<FloatLiteralNode>(tok, sc, val);
}

ExprPtr Ast::make_str_lit(Token* tok, size_t sc, const std::string& val) {
  return std::make_unique<StringLiteralNode>(tok, sc, val);
}

ExprPtr Ast::make_bool_lit(Token* tok, size_t sc, bool val) {
  return std::make_unique<BoolLiteralNode>(tok, sc, val);
}

ExprPtr Ast::make_null_lit(Token* tok, size_t sc) {
  return std::make_unique<NullLiteralNode>(tok, sc);
}

IdentPtr Ast::make_ident(Token* tok, size_t sc, const std::string& name) {
  return std::make_unique<IdentifierNode>(tok, sc, name);
}

ExprPtr Ast::make_binary_op(Token* tok, size_t sc, BinOperator op, ExprPtr lhs,
                            ExprPtr rhs) {
  return std::make_unique<BinaryOpExprNode>(tok, sc, op, std::move(lhs),
                                            std::move(rhs));
}

ExprPtr Ast::make_unary_op(Token* tok, size_t sc, UnaryOperator op,
                           ExprPtr expr) {
  return std::make_unique<UnaryExprNode>(tok, sc, op, std::move(expr));
}

std::unique_ptr<ArgumentNode> Ast::make_arg(Token* tok, size_t sc, bool give,
                                            ExprPtr expr) {
  return std::make_unique<ArgumentNode>(tok, sc, give, std::move(expr));
}

ExprPtr Ast::make_call(Token* tok, size_t sc, ExprPtr callee,
                       std::vector<std::unique_ptr<ArgumentNode>> args) {
  return std::make_unique<FunctionCallNode>(tok, sc, std::move(callee),
                                            std::move(args));
}

ExprPtr Ast::make_member_access(Token* tok, size_t sc, ExprPtr obj,
                                IdentPtr member) {
  return std::make_unique<MemberAccessNode>(tok, sc, std::move(obj),
                                            std::move(member));
}

ExprPtr Ast::make_array_index(Token* tok, size_t sc, ExprPtr arr, ExprPtr idx) {
  return std::make_unique<ArrayIndexNode>(tok, sc, std::move(arr),
                                          std::move(idx));
}

ExprPtr Ast::make_grouped(Token* tok, size_t sc, ExprPtr expr) {
  return std::make_unique<GroupedExprNode>(tok, sc, std::move(expr));
}

ExprPtr Ast::make_struct_literal(
    Token* tok, size_t sc, IdentPtr struct_type,
    std::vector<std::unique_ptr<StructFieldInitializerNode>> inits) {
  return std::make_unique<StructLiteralNode>(tok, sc, std::move(struct_type),
                                             std::move(inits));
}

std::unique_ptr<StructFieldInitializerNode> Ast::make_struct_initializer(
    Token* tok, size_t sc, IdentPtr field, ExprPtr value) {
  return std::make_unique<StructFieldInitializerNode>(tok, sc, std::move(field),
                                                      std::move(value));
}

ExprPtr Ast::make_new_expr(
    Token* tok, size_t sc, TypeKind allocated_type,
    std::variant<ExprPtr, std::optional<ExprPtr>> specifier) {
  return std::make_unique<NewExprNode>(tok, sc, std::move(allocated_type),
                                       std::move(specifier));
}

StmtPtr Ast::make_var_decl(Token* tok, size_t sc, bool mut, IdentPtr name,
                           std::optional<TypeKind> type, ExprPtr init) {
  return std::make_unique<VariableDeclNode>(tok, sc, mut, std::move(name),
                                            std::move(type), std::move(init));
}

StmtPtr Ast::make_assign(Token* tok, size_t sc, ExprPtr lhs, ExprPtr rhs) {
  return std::make_unique<AssignmentNode>(tok, sc, std::move(lhs),
                                          std::move(rhs));
}

BlockPtr Ast::make_block(Token* tok, size_t sc, std::vector<StmtPtr> stmts) {
  return std::make_unique<BlockNode>(tok, sc, std::move(stmts));
}

StmtPtr Ast::make_if(Token* tok, size_t sc, ExprPtr cond, BlockPtr then_blk,
                     BlockPtr else_blk) {
  return std::make_unique<IfStmtNode>(tok, sc, std::move(cond),
                                      std::move(then_blk), std::move(else_blk));
}

StmtPtr Ast::make_for(Token* tok, size_t sc, BlockPtr body, ExprPtr init,
                      ExprPtr cond, ExprPtr iter) {
  return std::make_unique<ForStmtNode>(tok, sc, std::move(body),
                                       std::move(init), std::move(cond),
                                       std::move(iter));
}

StmtPtr Ast::make_while(Token* tok, size_t sc, ExprPtr cond, BlockPtr body) {
  return std::make_unique<WhileStmtNode>(tok, sc, std::move(cond),
                                         std::move(body));
}

StmtPtr Ast::make_switch(Token* tok, size_t sc, ExprPtr expr,
                         std::vector<std::unique_ptr<CaseNode>> cases) {
  return std::make_unique<SwitchStmtNode>(tok, sc, std::move(expr),
                                          std::move(cases));
}

std::unique_ptr<CaseNode> Ast::make_case(Token* tok, size_t sc, BlockPtr body,
                                         ExprPtr value) {
  return std::make_unique<CaseNode>(tok, sc, std::move(body), std::move(value));
}

StmtPtr Ast::make_print(Token* tok, size_t sc, ExprPtr expr) {
  return std::make_unique<PrintStmtNode>(tok, sc, std::move(expr));
}

StmtPtr Ast::make_expr_stmt(Token* tok, size_t sc, ExprPtr expr) {
  return std::make_unique<ExpressionStatementNode>(tok, sc, std::move(expr));
}

StmtPtr Ast::make_return(Token* tok, size_t sc, ExprPtr value) {
  return std::make_unique<ReturnStmtNode>(tok, sc, std::move(value));
}

StmtPtr Ast::make_break(Token* tok, size_t sc) {
  return std::make_unique<BreakStmtNode>(tok, sc);
}

StmtPtr Ast::make_continue(Token* tok, size_t sc) {
  return std::make_unique<ContinueStmtNode>(tok, sc);
}

StmtPtr Ast::make_free(Token* tok, size_t sc, bool is_arr, ExprPtr expr) {
  return std::make_unique<FreeStmtNode>(tok, sc, is_arr, std::move(expr));
}

StmtPtr Ast::make_error(Token* tok, size_t sc, const std::string& msg) {
  return std::make_unique<ErrorStmtNode>(tok, sc, msg);
}

StmtPtr Ast::make_asm_block(Token* tok, size_t sc, const std::string& body) {
  return std::make_unique<AsmBlockNode>(tok, sc, body);
}

AstPtr Ast::make_struct_decl(
    Token* tok, size_t sc, IdentPtr name,
    std::vector<std::variant<std::unique_ptr<StructFieldNode>, AstPtr>>
        members) {
  return std::make_unique<StructDeclNode>(tok, sc, std::move(name),
                                          std::move(members));
}

std::unique_ptr<StructFieldNode> Ast::make_struct_field(Token* tok, size_t sc,
                                                        IdentPtr name,
                                                        TypeKind type) {
  return std::make_unique<StructFieldNode>(tok, sc, std::move(name),
                                           std::move(type));
}

AstPtr Ast::make_fn_decl(Token* tok, size_t sc, IdentPtr name,
                         std::vector<std::unique_ptr<ParamNode>> params,
                         BlockPtr body, std::optional<TypeKind> rt) {
  return std::make_unique<FunctionDeclNode>(tok, sc, std::move(name),
                                            std::move(params), std::move(body),
                                            std::move(rt));
}

std::unique_ptr<ParamNode> Ast::make_param(Token* tok, size_t sc,
                                           ParamModifier mod, IdentPtr name,
                                           TypeKind type) {
  return std::make_unique<ParamNode>(tok, sc, mod, std::move(name),
                                     std::move(type));
}

AstPtr Ast::make_program(Token* tok, size_t sc, std::vector<AstPtr> decls) {
  auto prog = std::make_unique<ProgramNode>(tok, sc);
  prog->top_level_declarations = std::move(decls);
  return prog;
}
