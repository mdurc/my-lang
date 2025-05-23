#include "visitor.h"

void AstPrinter::printIndent() {
  for (int i = 0; i < indent; ++i) out << "  ";
}

const char* AstPrinter::binOpToString(BinOperator op) {
  switch (op) {
    case BinOperator::Plus: return "+";
    case BinOperator::Minus: return "-";
    case BinOperator::Divide: return "/";
    case BinOperator::Modulo: return "%";
    case BinOperator::Multiply: return "*";
    case BinOperator::Equal: return "==";
    case BinOperator::NotEqual: return "!=";
    case BinOperator::GreaterThan: return ">";
    case BinOperator::GreaterEqual: return ">=";
    case BinOperator::LessThan: return "<";
    case BinOperator::LessEqual: return "<=";
    case BinOperator::LogicalAnd: return "&&";
    case BinOperator::LogicalOr: return "||";
    default: return "?";
  }
}

const char* AstPrinter::unaryOpToString(UnaryOperator op) {
  switch (op) {
    case UnaryOperator::Negate: return "-";
    case UnaryOperator::LogicalNot: return "!";
    case UnaryOperator::Dereference: return "*";
    case UnaryOperator::AddressOf: return "&";
    case UnaryOperator::AddressOfMut: return "&mut";
    default: return "?";
  }
}

const char* AstPrinter::borrowStateToString(BorrowState bs) {
  switch (bs) {
    case BorrowState::MutablyOwned: return "MutablyOwned";
    case BorrowState::ImmutableOwned: return "ImmutableOwned";
    case BorrowState::MutablyBorrowed: return "MutablyBorrowed";
    case BorrowState::ImmutablyBorrowed: return "ImmutablyBorrowed";
    default: return "UnknownBorrowState";
  }
}

void AstPrinter::printType(const Type& type) {
  printIndent();
  if (type.is<Type::Named>()) {
    out << "Type::Named(" << type.as<Type::Named>().identifier << ")";
  } else if (type.is<Type::Function>()) {
    out << "Type::Function(\n";
    indent++;
    printIndent();
    out << "Params: [\n";
    indent++;
    const auto& params = type.as<Type::Function>().parameters;
    for (size_t i = 0; i < params.size(); ++i) {
      printIndent();
      out << "Param(Modifier: " << borrowStateToString(params[i].first)
          << ", Type:\n";
      indent++;
      printType(*params[i].second);
      indent--;
      out << "\n";
      printIndent();
      out << ")";
      if (i < params.size() - 1) out << ",";
      out << "\n";
    }
    indent--;
    printIndent();
    out << "],\n";
    printIndent();
    out << "Return Type:\n";
    indent++;
    printType(*type.as<Type::Function>().return_type);
    indent--;
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  } else if (type.is<Type::Pointer>()) {
    out << "Type::Pointer(pointee mutable: "
        << (type.as<Type::Pointer>().is_pointee_mutable ? "true" : "false")
        << ",\n";
    indent++;
    printIndent();
    out << "Pointee:\n";
    indent++;
    printType(*type.as<Type::Pointer>().pointee);
    indent--;
    out << "\n";
    indent--;
    printIndent();
    out << ")";
  } else {
    out << "UnknownType";
  }
}

void print_ast(const AstPtr& node, std::ostream& out) {
  AstPrinter printer(out);
  node->accept(printer);
  out << std::endl;
}

void print_ast(const std::vector<AstPtr>& nodes, std::ostream& out) {
  AstPrinter printer(out);
  for (const auto& node : nodes) {
    node->accept(printer);
    out << std::endl;
  }
}
