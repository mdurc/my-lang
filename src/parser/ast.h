#ifndef PARSER_AST_H
#define PARSER_AST_H

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "../lexer/token.h"
#include "types.h"

template <typename T>
using ShrdPtr = std::shared_ptr<T>;
using AstPtr = ShrdPtr<class AstNode>;
using ExprPtr = ShrdPtr<class ExpressionNode>;
using StmtPtr = ShrdPtr<class StatementNode>;
using IdentPtr = ShrdPtr<class IdentifierNode>;
using BlockPtr = ShrdPtr<class BlockNode>;

using ArgPtr = ShrdPtr<class ArgumentNode>;
using StructFieldInitPtr = ShrdPtr<class StructFieldInitializerNode>;
using CasePtr = ShrdPtr<class CaseNode>;
using StructFieldPtr = ShrdPtr<class StructFieldNode>;
using ParamPtr = ShrdPtr<class ParamNode>;
using FuncDeclPtr = ShrdPtr<class FunctionDeclNode>;

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

#define DERIVE_NODE(Name, Base) \
  class Name : public Base {    \
    using Base::Base;           \
  };

#define LITERAL_NODE(Name, T)                    \
  class Name : public ExpressionNode {           \
  public:                                        \
    T value;                                     \
    Name(const Token* tok, size_t sc, T val)     \
        : ExpressionNode(tok, sc), value(val) {} \
  };

class AstNode {
public:
  const Token* token; // Primarily for location and error handling
  size_t scope_id;    // Id within the Symbol Table

  AstNode(const Token* tok, size_t sc) : token(tok), scope_id(sc) {}
  virtual ~AstNode() = default;
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
  IdentifierNode(const Token* tok, size_t sc, const std::string& name_val)
      : ExpressionNode(tok, sc), name(name_val) {}
};

class BinaryOpExprNode : public ExpressionNode {
public:
  BinOperator op_type;
  ExprPtr left;
  ExprPtr right;

  BinaryOpExprNode(const Token* tok, size_t sc, BinOperator op, ExprPtr l,
                   ExprPtr r)
      : ExpressionNode(tok, sc),
        op_type(op),
        left(std::move(l)),
        right(std::move(r)) {}
};

class UnaryExprNode : public ExpressionNode {
public:
  UnaryOperator op_type;
  ExprPtr operand;

  UnaryExprNode(const Token* tok, size_t sc, UnaryOperator op, ExprPtr expr)
      : ExpressionNode(tok, sc), op_type(op), operand(std::move(expr)) {}
};

class ArgumentNode : public AstNode {
public:
  bool is_give; // for 'give' keyword
  ExprPtr expression;

  ArgumentNode(const Token* tok, size_t sc, bool give, ExprPtr expr)
      : AstNode(tok, sc), is_give(give), expression(std::move(expr)) {}
};

class FunctionCallNode : public ExpressionNode {
public:
  ExprPtr callee;
  std::vector<ArgPtr> arguments;

  FunctionCallNode(const Token* tok, size_t sc, ExprPtr fn_callee,
                   std::vector<ArgPtr> args)
      : ExpressionNode(tok, sc),
        callee(std::move(fn_callee)),
        arguments(std::move(args)) {}
};

class MemberAccessNode : public ExpressionNode {
public:
  ExprPtr object;
  IdentPtr member;
  // <object>.<member>

  MemberAccessNode(const Token* tok, size_t sc, ExprPtr obj_expr,
                   IdentPtr member)
      : ExpressionNode(tok, sc),
        object(std::move(obj_expr)),
        member(std::move(member)) {}
};

class ArrayIndexNode : public ExpressionNode {
public:
  ExprPtr object;
  ExprPtr index;
  // <object>[<index>]

  ArrayIndexNode(const Token* tok, size_t sc, ExprPtr arr_expr,
                 ExprPtr idx_expr)
      : ExpressionNode(tok, sc),
        object(std::move(arr_expr)),
        index(std::move(idx_expr)) {}
};

class GroupedExprNode : public ExpressionNode {
public:
  ExprPtr expression;
  GroupedExprNode(const Token* tok, size_t sc, ExprPtr expr)
      : ExpressionNode(tok, sc), expression(std::move(expr)) {}
};

class StructFieldInitializerNode : public AstNode {
public:
  IdentPtr field;
  ExprPtr value;

  StructFieldInitializerNode(const Token* tok, size_t sc, IdentPtr field,
                             ExprPtr val)
      : AstNode(tok, sc), field(std::move(field)), value(std::move(val)) {}
};

class StructLiteralNode : public ExpressionNode {
public:
  IdentPtr struct_type;
  std::vector<StructFieldInitPtr> initializers;

  StructLiteralNode(const Token* tok, size_t sc, IdentPtr struct_type,
                    std::vector<StructFieldInitPtr> inits)
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

  NewExprNode(const Token* tok, size_t sc, TypeKind allocated_type,
              decltype(allocation_specifier) specifier)
      : ExpressionNode(tok, sc),
        type_to_allocate(std::move(allocated_type)),
        allocation_specifier(std::move(specifier)) {}
};

// == Statement Nodes ==
class StatementNode : public AstNode {
public:
  StatementNode(const Token* tok, size_t sc) : AstNode(tok, sc) {}
};

class VariableDeclNode : public StatementNode {
public:
  bool is_mutable;
  IdentPtr var_name;
  std::optional<TypeKind> type; // Optional for ':=' type inference
  ExprPtr initializer;          // optional

  VariableDeclNode(const Token* tok, size_t sc, bool mut, IdentPtr var_name,
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

  AssignmentNode(const Token* tok, size_t sc, ExprPtr lval, ExprPtr rval)
      : StatementNode(tok, sc),
        lvalue(std::move(lval)),
        rvalue(std::move(rval)) {}
};

class BlockNode : public StatementNode {
public:
  std::vector<StmtPtr> statements;

  BlockNode(const Token* tok, size_t sc, std::vector<StmtPtr> stmts = {})
      : StatementNode(tok, sc), statements(std::move(stmts)) {}
};

class IfStmtNode : public StatementNode {
public:
  ExprPtr condition;
  BlockPtr then_branch;
  BlockPtr else_branch; // optional

  IfStmtNode(const Token* tok, size_t sc, ExprPtr cond, BlockPtr then_b,
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

  ForStmtNode(const Token* tok, size_t sc, BlockPtr b, ExprPtr init = nullptr,
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

  WhileStmtNode(const Token* tok, size_t sc, ExprPtr cond, BlockPtr b)
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

  CaseNode(const Token* tok, size_t sc, BlockPtr b, ExprPtr val = nullptr)
      : AstNode(tok, sc), value(std::move(val)), body(std::move(b)) {}
};

class SwitchStmtNode : public StatementNode {
public:
  ExprPtr expression;
  std::vector<CasePtr> cases;

  SwitchStmtNode(const Token* tok, size_t sc, ExprPtr expr,
                 std::vector<CasePtr> c)
      : StatementNode(tok, sc),
        expression(std::move(expr)),
        cases(std::move(c)) {}
};

class PrintStmtNode : public StatementNode {
public:
  ExprPtr expression;
  PrintStmtNode(const Token* tok, size_t sc, ExprPtr expr)
      : StatementNode(tok, sc), expression(std::move(expr)) {}
};

class ExpressionStatementNode : public StatementNode {
public:
  ExprPtr expression;
  ExpressionStatementNode(const Token* tok, size_t sc, ExprPtr expr)
      : StatementNode(tok, sc), expression(std::move(expr)) {}
};

class ReturnStmtNode : public StatementNode {
public:
  ExprPtr value; // optional
  ReturnStmtNode(const Token* tok, size_t sc, ExprPtr val = nullptr)
      : StatementNode(tok, sc), value(std::move(val)) {}
};

class FreeStmtNode : public StatementNode {
public:
  bool is_array_deallocation;
  ExprPtr expression;
  FreeStmtNode(const Token* tok, size_t sc, bool is_arr, ExprPtr expr)
      : StatementNode(tok, sc),
        is_array_deallocation(is_arr),
        expression(std::move(expr)) {}
};

class ErrorStmtNode : public StatementNode {
public:
  std::string message_content;
  ErrorStmtNode(const Token* tok, size_t sc, const std::string& msg)
      : StatementNode(tok, sc), message_content(msg) {}
};

class AsmBlockNode : public StatementNode {
public:
  std::string body;
  AsmBlockNode(const Token* tok, size_t sc, const std::string& b)
      : StatementNode(tok, sc), body(b) {}
};

// == Declaration Nodes ==
class StructFieldNode : public AstNode {
public:
  IdentPtr name;
  TypeKind type;

  StructFieldNode(const Token* tok, size_t sc, IdentPtr name, TypeKind tk)
      : AstNode(tok, sc), name(std::move(name)), type(std::move(tk)) {}
};

class StructDeclNode : public AstNode {
public:
  IdentPtr name;

  // A struct member can be a field or a method (FunctionDecl)
  std::vector<std::variant<StructFieldPtr, FuncDeclPtr>> members;

  StructDeclNode(const Token* tok, size_t sc, IdentPtr name,
                 decltype(members) m)
      : AstNode(tok, sc), name(std::move(name)), members(std::move(m)) {}
};

class ParamNode : public AstNode {
public:
  BorrowState modifier;
  IdentPtr name;
  TypeKind type;

  ParamNode(const Token* tok, size_t sc, BorrowState mod, IdentPtr name,
            TypeKind tk)
      : AstNode(tok, sc),
        modifier(mod),
        name(std::move(name)),
        type(std::move(tk)) {}
};

class FunctionDeclNode : public AstNode {
public:
  IdentPtr name;
  std::vector<ParamPtr> params;
  std::optional<std::string> return_type_name;
  std::optional<TypeKind> return_type; // optional
  BlockPtr body;

  FunctionDeclNode(const Token* tok, size_t sc, IdentPtr name,
                   std::vector<ParamPtr> ps, BlockPtr b,
                   std::optional<std::string> rt_n = std::nullopt,
                   std::optional<TypeKind> rt = std::nullopt)
      : AstNode(tok, sc),
        name(std::move(name)),
        params(std::move(ps)),
        return_type_name(std::move(rt_n)),
        return_type(std::move(rt)),
        body(std::move(b)) {}
};

#endif // PARSER_AST_H
