#ifndef PARSER_AST_H
#define PARSER_AST_H

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "../lexer/token.h"
#include "types.h"

template <typename T>
using UniqPtr = std::unique_ptr<T>;
using AstPtr = UniqPtr<class AstNode>;
using ExprPtr = UniqPtr<class ExpressionNode>;
using StmtPtr = UniqPtr<class StatementNode>;
using IdentPtr = UniqPtr<class IdentifierNode>;
using BlockPtr = UniqPtr<class BlockNode>;

enum BinOperator {
  Plus,
  Minus,
  Divide,
  Modulo,
  Multiply,
  Equal,
  NotEqual,
  GreaterThan,
  GreaterEqual,
  LessThan,
  LessEqual,
  LogicalAnd,
  LogicalOr
};

enum UnaryOperator { Negate, LogicalNot, Dereference, AddressOf, AddressOfMut };
enum class ParamModifier { Mut, Read, Take };

#define DERIVE_NODE(Name, Base) \
  class Name : public Base {    \
    using Base::Base;           \
  };

#define LITERAL_NODE(Name, T)                    \
  class Name : public ExpressionNode {           \
  public:                                        \
    T value;                                     \
    Name(Token* tok, size_t sc, T val)           \
        : ExpressionNode(tok, sc), value(val) {} \
  };

class AstNode {
public:
  Token* token;    // Primarily for location and error handling
  size_t scope_id; // Id within the Symbol Table

  AstNode(Token* tok, size_t sc) : token(tok), scope_id(sc) {}
  virtual ~AstNode() = default;

  // TODO: visitor design pattern?
  // virtual void accept(AstVisitor& visitor) = 0;
};

// == Expression Nodes ==
DERIVE_NODE(ExpressionNode, AstNode);
LITERAL_NODE(IntegerLiteralNode, uint64_t)
LITERAL_NODE(FloatLiteralNode, double)
LITERAL_NODE(StringLiteralNode, std::string)
LITERAL_NODE(BoolLiteralNode, bool)
DERIVE_NODE(NullLiteralNode, ExpressionNode)

class IdentifierNode : public ExpressionNode {
public:
  std::string name;
  IdentifierNode(Token* tok, size_t sc, const std::string& name_val)
      : ExpressionNode(tok, sc), name(name_val) {}
};

class BinaryOpExprNode : public ExpressionNode {
public:
  BinOperator op_type;
  ExprPtr left;
  ExprPtr right;

  BinaryOpExprNode(Token* tok, size_t sc, BinOperator op, ExprPtr l, ExprPtr r)
      : ExpressionNode(tok, sc),
        op_type(op),
        left(std::move(l)),
        right(std::move(r)) {}
};

class UnaryExprNode : public ExpressionNode {
public:
  UnaryOperator op_type;
  ExprPtr operand;

  UnaryExprNode(Token* tok, size_t sc, UnaryOperator op, ExprPtr expr)
      : ExpressionNode(tok, sc), op_type(op), operand(std::move(expr)) {}
};

class ArgumentNode : public AstNode {
public:
  bool is_give; // for 'give' keyword
  ExprPtr expression;

  ArgumentNode(Token* tok, size_t sc, bool give, ExprPtr expr)
      : AstNode(tok, sc), is_give(give), expression(std::move(expr)) {}
};

class FunctionCallNode : public ExpressionNode {
public:
  ExprPtr callee;
  std::vector<std::unique_ptr<ArgumentNode>> arguments;

  FunctionCallNode(Token* tok, size_t sc, ExprPtr fn_callee,
                   std::vector<std::unique_ptr<ArgumentNode>> args)
      : ExpressionNode(tok, sc),
        callee(std::move(fn_callee)),
        arguments(std::move(args)) {}
};

class MemberAccessNode : public ExpressionNode {
public:
  ExprPtr object;
  IdentPtr member;
  // <object>.<member>

  MemberAccessNode(Token* tok, size_t sc, ExprPtr obj_expr, IdentPtr member)
      : ExpressionNode(tok, sc),
        object(std::move(obj_expr)),
        member(std::move(member)) {}
};

class ArrayIndexNode : public ExpressionNode {
public:
  ExprPtr object;
  ExprPtr index;
  // <object>[<index>]

  ArrayIndexNode(Token* tok, size_t sc, ExprPtr arr_expr, ExprPtr idx_expr)
      : ExpressionNode(tok, sc),
        object(std::move(arr_expr)),
        index(std::move(idx_expr)) {}
};

class GroupedExprNode : public ExpressionNode {
public:
  ExprPtr expression;
  GroupedExprNode(Token* tok, size_t sc, ExprPtr expr)
      : ExpressionNode(tok, sc), expression(std::move(expr)) {}
};

class StructFieldInitializerNode : public AstNode {
public:
  IdentPtr field;
  ExprPtr value;

  StructFieldInitializerNode(Token* tok, size_t sc, IdentPtr field, ExprPtr val)
      : AstNode(tok, sc), field(std::move(field)), value(std::move(val)) {}
};

class StructLiteralNode : public ExpressionNode {
public:
  IdentPtr struct_type;
  std::vector<std::unique_ptr<StructFieldInitializerNode>> initializers;

  StructLiteralNode(
      Token* tok, size_t sc, IdentPtr struct_type,
      std::vector<std::unique_ptr<StructFieldInitializerNode>> inits)
      : ExpressionNode(tok, sc),
        struct_type(std::move(struct_type)),
        initializers(std::move(inits)) {}
};

class NewExprNode : public ExpressionNode {
public:
  TypeKind type_to_allocate;           // The <Type> part
  std::variant<ExprPtr,                // Array size
               std::optional<ExprPtr>> // Constructor arg
      allocation_specifier;

  NewExprNode(Token* tok, size_t sc, TypeKind allocated_type,
              decltype(allocation_specifier) specifier)
      : ExpressionNode(tok, sc),
        type_to_allocate(std::move(allocated_type)),
        allocation_specifier(std::move(specifier)) {}
};

// == Statement Nodes ==
class StatementNode : public AstNode {
public:
  StatementNode(Token* tok, size_t sc) : AstNode(tok, sc) {}
};

class VariableDeclNode : public StatementNode {
public:
  bool is_mutable;
  IdentPtr var_name;
  std::optional<TypeKind> type; // Optional for ':=' type inference
  ExprPtr initializer;          // optional

  VariableDeclNode(Token* tok, size_t sc, bool mut, IdentPtr var_name,
                   std::optional<TypeKind> tk, ExprPtr init = nullptr)
      : StatementNode(tok, sc),
        is_mutable(mut),
        var_name(std::move(var_name)),
        type(std::move(tk)),
        initializer(std::move(init)) {}
};

class AssignmentNode : public StatementNode {
public:
  ExprPtr lvalue;
  ExprPtr rvalue;

  AssignmentNode(Token* tok, size_t sc, ExprPtr lval, ExprPtr rval)
      : StatementNode(tok, sc),
        lvalue(std::move(lval)),
        rvalue(std::move(rval)) {}
};

class BlockNode : public StatementNode {
public:
  std::vector<StmtPtr> statements;

  BlockNode(Token* tok, size_t sc, std::vector<StmtPtr> stmts = {})
      : StatementNode(tok, sc), statements(std::move(stmts)) {}
};

class IfStmtNode : public StatementNode {
public:
  ExprPtr condition;
  BlockPtr then_branch;
  BlockPtr else_branch; // optional

  IfStmtNode(Token* tok, size_t sc, ExprPtr cond, BlockPtr then_b,
             BlockPtr else_b = nullptr)
      : StatementNode(tok, sc),
        condition(std::move(cond)),
        then_branch(std::move(then_b)),
        else_branch(std::move(else_b)) {}
};

class ForStmtNode : public StatementNode {
public:
  ExprPtr initializer;
  ExprPtr condition;
  ExprPtr iteration;
  BlockPtr body;

  ForStmtNode(Token* tok, size_t sc, BlockPtr b, ExprPtr init = nullptr,
              ExprPtr cond = nullptr, ExprPtr iter = nullptr)
      : StatementNode(tok, sc),
        initializer(std::move(init)),
        condition(std::move(cond)),
        iteration(std::move(iter)),
        body(std::move(b)) {}
};

class WhileStmtNode : public StatementNode {
public:
  ExprPtr condition;
  BlockPtr body;

  WhileStmtNode(Token* tok, size_t sc, ExprPtr cond, BlockPtr b)
      : StatementNode(tok, sc),
        condition(std::move(cond)),
        body(std::move(b)) {}
};

DERIVE_NODE(BreakStmtNode, StatementNode)
DERIVE_NODE(ContinueStmtNode, StatementNode)

class CaseNode : public AstNode {
public:
  ExprPtr value; // nullptr for default
  BlockPtr body;

  CaseNode(Token* tok, size_t sc, BlockPtr b, ExprPtr val = nullptr)
      : AstNode(tok, sc), value(std::move(val)), body(std::move(b)) {}
};

class SwitchStmtNode : public StatementNode {
public:
  ExprPtr expression;
  std::vector<std::unique_ptr<CaseNode>> cases;

  SwitchStmtNode(Token* tok, size_t sc, ExprPtr expr,
                 std::vector<std::unique_ptr<CaseNode>> c)
      : StatementNode(tok, sc),
        expression(std::move(expr)),
        cases(std::move(c)) {}
};

class PrintStmtNode : public StatementNode {
public:
  ExprPtr expression;
  PrintStmtNode(Token* tok, size_t sc, ExprPtr expr)
      : StatementNode(tok, sc), expression(std::move(expr)) {}
};

class ExpressionStatementNode : public StatementNode {
public:
  ExprPtr expression;
  ExpressionStatementNode(Token* tok, size_t sc, ExprPtr expr)
      : StatementNode(tok, sc), expression(std::move(expr)) {}
};

class ReturnStmtNode : public StatementNode {
public:
  ExprPtr value; // optional
  ReturnStmtNode(Token* tok, size_t sc, ExprPtr val = nullptr)
      : StatementNode(tok, sc), value(std::move(val)) {}
};

class FreeStmtNode : public StatementNode {
public:
  bool is_array_deallocation;
  ExprPtr expression;
  FreeStmtNode(Token* tok, size_t sc, bool is_arr, ExprPtr expr)
      : StatementNode(tok, sc),
        is_array_deallocation(is_arr),
        expression(std::move(expr)) {}
};

class ErrorStmtNode : public StatementNode {
public:
  std::string message_content;
  ErrorStmtNode(Token* tok, size_t sc, const std::string& msg)
      : StatementNode(tok, sc), message_content(msg) {}
};

class AsmBlockNode : public StatementNode {
public:
  std::string body;
  AsmBlockNode(Token* tok, size_t sc, const std::string& b)
      : StatementNode(tok, sc), body(b) {}
};

// == Declaration Nodes ==
class StructFieldNode : public AstNode {
public:
  IdentPtr name;
  TypeKind type;

  StructFieldNode(Token* tok, size_t sc, IdentPtr name, TypeKind tk)
      : AstNode(tok, sc), name(std::move(name)), type(std::move(tk)) {}
};

class StructDeclNode : public AstNode {
public:
  IdentPtr name;

  // A struct member can be a field or a method (FunctionDecl)
  std::vector<std::variant<std::unique_ptr<StructFieldNode>, AstPtr>> members;

  StructDeclNode(Token* tok, size_t sc, IdentPtr name, decltype(members) m)
      : AstNode(tok, sc), name(std::move(name)), members(std::move(m)) {}
};

class ParamNode : public AstNode {
public:
  ParamModifier modifier;
  IdentPtr name;
  TypeKind type;

  ParamNode(Token* tok, size_t sc, ParamModifier mod, IdentPtr name,
            TypeKind tk)
      : AstNode(tok, sc),
        modifier(mod),
        name(std::move(name)),
        type(std::move(tk)) {}
};

class FunctionDeclNode : public AstNode {
public:
  IdentPtr name;
  std::vector<std::unique_ptr<ParamNode>> params;
  std::optional<TypeKind> return_type; // optional
  BlockPtr body;

  FunctionDeclNode(Token* tok, size_t sc, IdentPtr name,
                   std::vector<std::unique_ptr<ParamNode>> ps, BlockPtr b,
                   std::optional<TypeKind> rt = std::nullopt)
      : AstNode(tok, sc),
        name(std::move(name)),
        params(std::move(ps)),
        return_type(std::move(rt)),
        body(std::move(b)) {}
};

// == Program Node ==
class ProgramNode : public AstNode {
public:
  std::vector<AstPtr> top_level_declarations;

  ProgramNode(Token* tok, size_t sc) : AstNode(tok, sc) {}
};

class Ast {
public:
  Ast() = delete;
  // Literals
  static ExprPtr make_int_lit(Token* tok, size_t sc, uint64_t val);
  static ExprPtr make_float_lit(Token* tok, size_t sc, double val);
  static ExprPtr make_str_lit(Token* tok, size_t sc, const std::string& val);
  static ExprPtr make_bool_lit(Token* tok, size_t sc, bool val);
  static ExprPtr make_null_lit(Token* tok, size_t sc);

  // Identifiers
  static IdentPtr make_ident(Token* tok, size_t sc, const std::string& name);

  // Expressions
  static ExprPtr make_binary_op(Token* tok, size_t sc, BinOperator op,
                                ExprPtr lhs, ExprPtr rhs);
  static ExprPtr make_unary_op(Token* tok, size_t sc, UnaryOperator op,
                               ExprPtr expr);
  static std::unique_ptr<ArgumentNode> make_arg(Token* tok, size_t sc,
                                                bool give, ExprPtr expr);
  static ExprPtr make_call(Token* tok, size_t sc, ExprPtr callee,
                           std::vector<std::unique_ptr<ArgumentNode>> args);
  static ExprPtr make_member_access(Token* tok, size_t sc, ExprPtr obj,
                                    IdentPtr member);
  static ExprPtr make_array_index(Token* tok, size_t sc, ExprPtr arr,
                                  ExprPtr idx);
  static ExprPtr make_grouped(Token* tok, size_t sc, ExprPtr expr);
  static ExprPtr make_struct_literal(
      Token* tok, size_t sc, IdentPtr struct_type,
      std::vector<std::unique_ptr<StructFieldInitializerNode>> inits);
  static std::unique_ptr<StructFieldInitializerNode> make_struct_initializer(
      Token* tok, size_t sc, IdentPtr field, ExprPtr value);
  static ExprPtr make_new_expr(
      Token* tok, size_t sc, TypeKind allocated_type,
      std::variant<ExprPtr, std::optional<ExprPtr>> specifier);

  // Statements
  static StmtPtr make_var_decl(Token* tok, size_t sc, bool mut, IdentPtr name,
                               std::optional<TypeKind> type,
                               ExprPtr init = nullptr);
  static StmtPtr make_assign(Token* tok, size_t sc, ExprPtr lhs, ExprPtr rhs);
  static BlockPtr make_block(Token* tok, size_t sc,
                             std::vector<StmtPtr> stmts = {});
  static StmtPtr make_if(Token* tok, size_t sc, ExprPtr cond, BlockPtr then_blk,
                         BlockPtr else_blk = nullptr);
  static StmtPtr make_for(Token* tok, size_t sc, BlockPtr body,
                          ExprPtr init = nullptr, ExprPtr cond = nullptr,
                          ExprPtr iter = nullptr);
  static StmtPtr make_while(Token* tok, size_t sc, ExprPtr cond, BlockPtr body);
  static StmtPtr make_switch(Token* tok, size_t sc, ExprPtr expr,
                             std::vector<std::unique_ptr<CaseNode>> cases);
  static std::unique_ptr<CaseNode> make_case(Token* tok, size_t sc,
                                             BlockPtr body,
                                             ExprPtr value = nullptr);
  static StmtPtr make_print(Token* tok, size_t sc, ExprPtr expr);
  static StmtPtr make_expr_stmt(Token* tok, size_t sc, ExprPtr expr);
  static StmtPtr make_return(Token* tok, size_t sc, ExprPtr value = nullptr);
  static StmtPtr make_break(Token* tok, size_t sc);
  static StmtPtr make_continue(Token* tok, size_t sc);
  static StmtPtr make_free(Token* tok, size_t sc, bool is_arr, ExprPtr expr);
  static StmtPtr make_error(Token* tok, size_t sc, const std::string& msg);
  static StmtPtr make_asm_block(Token* tok, size_t sc, const std::string& body);

  // Declarations
  static AstPtr make_struct_decl(
      Token* tok, size_t sc, IdentPtr name,
      std::vector<std::variant<std::unique_ptr<StructFieldNode>, AstPtr>>
          members);
  static std::unique_ptr<StructFieldNode> make_struct_field(Token* tok,
                                                            size_t sc,
                                                            IdentPtr name,
                                                            TypeKind type);
  static AstPtr make_fn_decl(Token* tok, size_t sc, IdentPtr name,
                             std::vector<std::unique_ptr<ParamNode>> params,
                             BlockPtr body,
                             std::optional<TypeKind> rt = std::nullopt);
  static std::unique_ptr<ParamNode> make_param(Token* tok, size_t sc,
                                               ParamModifier mod, IdentPtr name,
                                               TypeKind type);

  // Program
  static AstPtr make_program(Token* tok, size_t sc, std::vector<AstPtr> decls);
};

#endif // PARSER_AST_H
