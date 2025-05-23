#include "ast.h"

#include "visitor.h"

void IntegerLiteralNode::accept(Visitor& v) const { v.visit(*this); }
void FloatLiteralNode::accept(Visitor& v) const { v.visit(*this); }
void StringLiteralNode::accept(Visitor& v) const { v.visit(*this); }
void BoolLiteralNode::accept(Visitor& v) const { v.visit(*this); }
void NullLiteralNode::accept(Visitor& v) const { v.visit(*this); }
void BreakStmtNode::accept(Visitor& v) const { v.visit(*this); }
void ContinueStmtNode::accept(Visitor& v) const { v.visit(*this); }
void IdentifierNode::accept(Visitor& v) const { v.visit(*this); }
void AssignmentNode::accept(Visitor& v) const { v.visit(*this); }
void BinaryOpExprNode::accept(Visitor& v) const { v.visit(*this); }
void UnaryExprNode::accept(Visitor& v) const { v.visit(*this); }
void ArgumentNode::accept(Visitor& v) const { v.visit(*this); }
void FunctionCallNode::accept(Visitor& v) const { v.visit(*this); }
void MemberAccessNode::accept(Visitor& v) const { v.visit(*this); }
void ArrayIndexNode::accept(Visitor& v) const { v.visit(*this); }
void GroupedExprNode::accept(Visitor& v) const { v.visit(*this); }
void StructFieldInitializerNode::accept(Visitor& v) const { v.visit(*this); }
void StructLiteralNode::accept(Visitor& v) const { v.visit(*this); }
void NewExprNode::accept(Visitor& v) const { v.visit(*this); }
void VariableDeclNode::accept(Visitor& v) const { v.visit(*this); }
void BlockNode::accept(Visitor& v) const { v.visit(*this); }
void IfStmtNode::accept(Visitor& v) const { v.visit(*this); }
void ForStmtNode::accept(Visitor& v) const { v.visit(*this); }
void WhileStmtNode::accept(Visitor& v) const { v.visit(*this); }
void CaseNode::accept(Visitor& v) const { v.visit(*this); }
void SwitchStmtNode::accept(Visitor& v) const { v.visit(*this); }
void PrintStmtNode::accept(Visitor& v) const { v.visit(*this); }
void ExpressionStatementNode::accept(Visitor& v) const { v.visit(*this); }
void ReturnStmtNode::accept(Visitor& v) const { v.visit(*this); }
void FreeStmtNode::accept(Visitor& v) const { v.visit(*this); }
void ErrorStmtNode::accept(Visitor& v) const { v.visit(*this); }
void AsmBlockNode::accept(Visitor& v) const { v.visit(*this); }
void StructFieldNode::accept(Visitor& v) const { v.visit(*this); }
void StructDeclNode::accept(Visitor& v) const { v.visit(*this); }
void ParamNode::accept(Visitor& v) const { v.visit(*this); }
void FunctionDeclNode::accept(Visitor& v) const { v.visit(*this); }
