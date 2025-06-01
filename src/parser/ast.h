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

class Visitor;

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

#define DERIVE_NODE(Name, Base)       \
  class Name : public Base {          \
    using Base::Base;                 \
    void accept(Visitor& v) override; \
  };

#define DERIVE_ABSTRACT_NODE(Name, Base) \
  class Name : public Base {             \
    using Base::Base;                    \
  };

#define LITERAL_NODE(Name, T)                    \
  class Name : public ExpressionNode {           \
  public:                                        \
    T value;                                     \
    Name(const Token* tok, size_t sc, T val)     \
        : ExpressionNode(tok, sc), value(val) {} \
    void accept(Visitor& v) override;            \
  };

class AstNode {
public:
  const Token* token; // Primarily for location and error handling
  size_t scope_id;    // Id within the Symbol Table

  AstNode(const Token* tok, size_t sc) : token(tok), scope_id(sc) {}
  virtual ~AstNode() = default;
  virtual void accept(Visitor& v) = 0;
};

// == Base Abstract Nodes (Expressions and Statements) ==
class ExpressionNode : public AstNode {
public:
  std::shared_ptr<Type> expr_type = nullptr; // filled by type checker
  using AstNode::AstNode;
};
DERIVE_ABSTRACT_NODE(StatementNode, AstNode)

// == Literal Nodes ==
LITERAL_NODE(IntegerLiteralNode, uint64_t)
LITERAL_NODE(FloatLiteralNode, double)
LITERAL_NODE(StringLiteralNode, std::string)
LITERAL_NODE(BoolLiteralNode, bool)
DERIVE_NODE(NullLiteralNode, ExpressionNode)

// == Basic Nodes ==
DERIVE_NODE(BreakStmtNode, StatementNode)
DERIVE_NODE(ContinueStmtNode, StatementNode)

// == Expression Nodes ==
class IdentifierNode : public ExpressionNode {
public:
  std::string name;
  IdentifierNode(const Token* tok, size_t sc, std::string name_val)
      : ExpressionNode(tok, sc), name(std::move(name_val)) {}
  void accept(Visitor& v) override;
};

class AssignmentNode : public ExpressionNode {
public:
  ExprPtr lvalue;
  ExprPtr rvalue;

  AssignmentNode(const Token* tok, size_t sc, ExprPtr lval, ExprPtr rval)
      : ExpressionNode(tok, sc), lvalue(lval), rvalue(rval) {}
  void accept(Visitor& v) override;
};

class BinaryOpExprNode : public ExpressionNode {
public:
  BinOperator op_type;
  ExprPtr left;
  ExprPtr right;

  BinaryOpExprNode(const Token* tok, size_t sc, BinOperator op, ExprPtr l,
                   ExprPtr r)
      : ExpressionNode(tok, sc), op_type(op), left(l), right(r) {}
  void accept(Visitor& v) override;
};

class UnaryExprNode : public ExpressionNode {
public:
  UnaryOperator op_type;
  ExprPtr operand;

  UnaryExprNode(const Token* tok, size_t sc, UnaryOperator op, ExprPtr expr)
      : ExpressionNode(tok, sc), op_type(op), operand(expr) {}
  void accept(Visitor& v) override;
};

class ArgumentNode : public AstNode {
public:
  bool is_give; // for 'give' keyword
  ExprPtr expression;

  ArgumentNode(const Token* tok, size_t sc, bool give, ExprPtr expr)
      : AstNode(tok, sc), is_give(give), expression(expr) {}
  void accept(Visitor& v) override;
};

class FunctionCallNode : public ExpressionNode {
public:
  ExprPtr callee;
  std::vector<ArgPtr> arguments;

  FunctionCallNode(const Token* tok, size_t sc, ExprPtr fn_callee,
                   std::vector<ArgPtr> args)
      : ExpressionNode(tok, sc),
        callee(fn_callee),
        arguments(std::move(args)) {}
  void accept(Visitor& v) override;
};

class MemberAccessNode : public ExpressionNode {
public:
  ExprPtr object;
  IdentPtr member;
  // <object>.<member>

  MemberAccessNode(const Token* tok, size_t sc, ExprPtr obj_expr,
                   IdentPtr member)
      : ExpressionNode(tok, sc), object(obj_expr), member(member) {}
  void accept(Visitor& v) override;
};

class ArrayIndexNode : public ExpressionNode {
public:
  ExprPtr object;
  ExprPtr index;
  // <object>[<index>]

  ArrayIndexNode(const Token* tok, size_t sc, ExprPtr arr_expr,
                 ExprPtr idx_expr)
      : ExpressionNode(tok, sc), object(arr_expr), index(idx_expr) {}
  void accept(Visitor& v) override;
};

class GroupedExprNode : public ExpressionNode {
public:
  ExprPtr expression;
  GroupedExprNode(const Token* tok, size_t sc, ExprPtr expr)
      : ExpressionNode(tok, sc), expression(expr) {}
  void accept(Visitor& v) override;
};

class StructFieldInitializerNode : public AstNode {
public:
  IdentPtr field;
  ExprPtr value;

  StructFieldInitializerNode(const Token* tok, size_t sc, IdentPtr field,
                             ExprPtr val)
      : AstNode(tok, sc), field(field), value(val) {}
  void accept(Visitor& v) override;
};

class StructLiteralNode : public ExpressionNode {
public:
  // the parser will resolve this when looking if the current token exists
  // as a Type in the symbol table, if so, it will be a Type::Named: struct_type
  std::shared_ptr<Type> struct_type;
  std::vector<StructFieldInitPtr> initializers;

  StructLiteralNode(const Token* tok, size_t sc,
                    std::shared_ptr<Type> struct_type,
                    std::vector<StructFieldInitPtr> inits)
      : ExpressionNode(tok, sc),
        struct_type(struct_type),
        initializers(std::move(inits)) {}
  void accept(Visitor& v) override;
};

class NewExprNode : public ExpressionNode {
public:
  bool is_memory_mutable;
  bool is_array_allocation;
  std::shared_ptr<Type> type_to_allocate; // The <Type> part
  ExprPtr allocation_specifier;

  NewExprNode(const Token* tok, size_t sc, bool is_mut, bool is_array,
              std::shared_ptr<Type> allocated_type, ExprPtr specifier)
      : ExpressionNode(tok, sc),
        is_memory_mutable(is_mut),
        is_array_allocation(is_array),
        type_to_allocate(allocated_type),
        allocation_specifier(specifier) {}
  void accept(Visitor& v) override;
};

// == Statement Nodes ==
class VariableDeclNode : public StatementNode {
public:
  bool is_mutable;
  IdentPtr var_name;
  std::shared_ptr<Type> type; // optional for ':=' type inference
  ExprPtr initializer;        // optional

  VariableDeclNode(const Token* tok, size_t sc, bool mut, IdentPtr var_name,
                   std::shared_ptr<Type> tk, ExprPtr init)
      : StatementNode(tok, sc),
        is_mutable(mut),
        var_name(var_name),
        type(tk),
        initializer(init) {}
  void accept(Visitor& v) override;
};

class BlockNode : public StatementNode {
public:
  std::vector<StmtPtr> statements;

  BlockNode(const Token* tok, size_t sc, std::vector<StmtPtr> stmts)
      : StatementNode(tok, sc), statements(std::move(stmts)) {}
  void accept(Visitor& v) override;
};

class IfStmtNode : public StatementNode {
public:
  ExprPtr condition;
  BlockPtr then_branch;
  BlockPtr else_branch; // optional

  IfStmtNode(const Token* tok, size_t sc, ExprPtr cond, BlockPtr then_b,
             BlockPtr else_b)
      : StatementNode(tok, sc),
        condition(cond),
        then_branch(then_b),
        else_branch(else_b) {}
  void accept(Visitor& v) override;
};

class ForStmtNode : public StatementNode {
public:
  std::optional<std::variant<ExprPtr, StmtPtr>>
      initializer;   // has_value ==> non-nullptr
  ExprPtr condition; // optional
  ExprPtr iteration; // optional
  BlockPtr body;

  ForStmtNode(const Token* tok, size_t sc,
              std::optional<std::variant<ExprPtr, StmtPtr>> init, ExprPtr cond,
              ExprPtr iter, BlockPtr b)
      : StatementNode(tok, sc),
        initializer(std::move(init)),
        condition(cond),
        iteration(iter),
        body(b) {}
  void accept(Visitor& v) override;
};

class WhileStmtNode : public StatementNode {
public:
  ExprPtr condition;
  BlockPtr body;

  WhileStmtNode(const Token* tok, size_t sc, ExprPtr cond, BlockPtr b)
      : StatementNode(tok, sc), condition(cond), body(b) {}
  void accept(Visitor& v) override;
};

class CaseNode : public AstNode {
public:
  ExprPtr value; // nullptr for default case
  BlockPtr body;

  CaseNode(const Token* tok, size_t sc, BlockPtr b, ExprPtr val)
      : AstNode(tok, sc), value(val), body(b) {}
  void accept(Visitor& v) override;
};

class SwitchStmtNode : public StatementNode {
public:
  ExprPtr expression;
  std::vector<CasePtr> cases;

  SwitchStmtNode(const Token* tok, size_t sc, ExprPtr expr,
                 std::vector<CasePtr> c)
      : StatementNode(tok, sc), expression(expr), cases(std::move(c)) {}
  void accept(Visitor& v) override;
};

class ReadStmtNode : public StatementNode {
public:
  ExprPtr expression; // identifier to store stdin into
  ReadStmtNode(const Token* tok, size_t sc, ExprPtr expr)
      : StatementNode(tok, sc), expression(expr) {}
  void accept(Visitor& v) override;
};

class PrintStmtNode : public StatementNode {
public:
  ExprPtr expression;
  PrintStmtNode(const Token* tok, size_t sc, ExprPtr expr)
      : StatementNode(tok, sc), expression(expr) {}
  void accept(Visitor& v) override;
};

class ExpressionStatementNode : public StatementNode {
public:
  ExprPtr expression;
  ExpressionStatementNode(const Token* tok, size_t sc, ExprPtr expr)
      : StatementNode(tok, sc), expression(expr) {}
  void accept(Visitor& v) override;
};

class ReturnStmtNode : public StatementNode {
public:
  ExprPtr value; // optional
  ReturnStmtNode(const Token* tok, size_t sc, ExprPtr val)
      : StatementNode(tok, sc), value(val) {}
  void accept(Visitor& v) override;
};

class FreeStmtNode : public StatementNode {
public:
  bool is_array_deallocation;
  ExprPtr expression;
  FreeStmtNode(const Token* tok, size_t sc, bool is_arr, ExprPtr expr)
      : StatementNode(tok, sc),
        is_array_deallocation(is_arr),
        expression(expr) {}
  void accept(Visitor& v) override;
};

class ErrorStmtNode : public StatementNode {
public:
  std::string message_content;
  ErrorStmtNode(const Token* tok, size_t sc, std::string msg)
      : StatementNode(tok, sc), message_content(std::move(msg)) {}
  void accept(Visitor& v) override;
};

class AsmBlockNode : public StatementNode {
public:
  std::string body;
  AsmBlockNode(const Token* tok, size_t sc, std::string b)
      : StatementNode(tok, sc), body(std::move(b)) {}
  void accept(Visitor& v) override;
};

// == Declaration Nodes ==
class StructFieldNode : public AstNode {
public:
  IdentPtr name;
  std::shared_ptr<Type> type;

  StructFieldNode(const Token* tok, size_t sc, IdentPtr name,
                  std::shared_ptr<Type> tk)
      : AstNode(tok, sc), name(name), type(tk) {}
  void accept(Visitor& v) override;
};

class StructDeclNode : public AstNode {
public:
  // the custom user defined type that is this struct
  std::shared_ptr<Type> type;

  // A struct member can be a field or a method (FunctionDecl)
  std::vector<std::variant<StructFieldPtr, FuncDeclPtr>> members;

  StructDeclNode(const Token* tok, size_t sc, std::shared_ptr<Type> type,
                 decltype(members) m)
      : AstNode(tok, sc), type(type), members(std::move(m)) {}
  void accept(Visitor& v) override;
};

class ParamNode : public AstNode {
public:
  BorrowState modifier;
  IdentPtr name;
  std::shared_ptr<Type> type;

  ParamNode(const Token* tok, size_t sc, BorrowState mod, IdentPtr name,
            std::shared_ptr<Type> tk)
      : AstNode(tok, sc), modifier(mod), name(name), type(tk) {}
  void accept(Visitor& v) override;
};

class FunctionDeclNode : public AstNode {
public:
  IdentPtr name;
  std::vector<ParamPtr> params;
  std::optional<std::string> return_type_name; // optional for u0 return type
  std::shared_ptr<Type> return_type;
  BlockPtr body;
  std::shared_ptr<Type> type;

  FunctionDeclNode(const Token* tok, size_t sc, IdentPtr name,
                   std::vector<ParamPtr> ps, std::optional<std::string> rt_n,
                   std::shared_ptr<Type> rt, BlockPtr b,
                   std::shared_ptr<Type> t)
      : AstNode(tok, sc),
        name(name),
        params(std::move(ps)),
        return_type_name(std::move(rt_n)),
        return_type(rt),
        body(b),
        type(t) {}
  void accept(Visitor& v) override;
};

#endif // PARSER_AST_H
