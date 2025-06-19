#include "diagnostic.h"

#include <cstdlib>
#include <sstream>

Diagnostic::Diagnostic(DiagType type, const std::string& msg)
    : m_type(type), m_span(), m_msg(msg) {
  generate_message();
}
Diagnostic::Diagnostic(DiagType type, const Span& span, const std::string& msg)
    : m_type(type), m_span(span), m_msg(msg) {
  generate_message();
}

const char* Diagnostic::what() const noexcept {
  return m_formatted_msg.c_str();
}

std::string Diagnostic::format_type() const {
  switch (m_type) {
    case DiagType::FATAL_ERROR: return "[FATAL_ERROR]";
    case DiagType::ERROR: return "[ERROR]";
    case DiagType::WARNING: return "[WARNING]";
    case DiagType::HINT: return "[HINT]";
  }
  return "[UNKNOWN_TYPE]";
}

// == Specific Errors: ==

// ExpectedToken during parsing
ExpectedToken::ExpectedToken(const Span& span, TokenType expected,
                             TokenType got)
    : ExpectedToken(span, token_type_to_string(expected),
                    token_type_to_string(got)) {}
ExpectedToken::ExpectedToken(const Span& span, const std::string& expected,
                             TokenType got)
    : ExpectedToken(span, expected, token_type_to_string(got)) {}
ExpectedToken::ExpectedToken(const Span& span, const std::string& expected,
                             const std::string& got)
    : Diagnostic(DiagType::ERROR, span), m_expected(expected), m_got(got) {
  generate_message();
}

void ExpectedToken::generate_message() {
  std::stringstream ss;
  ss << format_type() << " " << m_span << ": Expected token '" << m_expected
     << "', got '" << m_got << "'";
  m_formatted_msg = ss.str();
}

// TypeMismatchError
TypeMismatchError::TypeMismatchError(const Span& span,
                                     const std::string& expected,
                                     const std::string& got)
    : Diagnostic(DiagType::ERROR, span), m_expected(expected), m_got(got) {
  generate_message();
}

void TypeMismatchError::generate_message() {
  std::stringstream ss;
  ss << format_type() << " " << m_span << ": Type mismatch: Expected '"
     << m_expected << "', got '" << m_got << "'";
  m_formatted_msg = ss.str();
}

// VariableNotFoundError
VariableNotFoundError::VariableNotFoundError(const Span& span,
                                             const std::string& identifier)
    : Diagnostic(DiagType::ERROR, span), m_identifier(identifier) {
  generate_message();
}

void VariableNotFoundError::generate_message() {
  std::stringstream ss;
  ss << format_type() << " " << m_span << ": Variable Not Found: identifier '"
     << m_identifier << "'";
  m_formatted_msg = ss.str();
}

// TypeNotFoundError
TypeNotFoundError::TypeNotFoundError(const Span& span, const std::string& type)
    : Diagnostic(DiagType::ERROR, span), m_type(type) {
  generate_message();
}

void TypeNotFoundError::generate_message() {
  std::stringstream ss;
  ss << format_type() << " " << m_span << ": Type Not Found: '" << m_type
     << "'";
  m_formatted_msg = ss.str();
}

// DuplicateDeclarationError
DuplicateDeclarationError::DuplicateDeclarationError(
    const Span& span, const std::string& identifier)
    : Diagnostic(DiagType::ERROR, span), m_identifier(identifier) {
  generate_message();
}

void DuplicateDeclarationError::generate_message() {
  std::stringstream ss;
  ss << format_type() << " " << m_span
     << ": Duplicate Declaration: identifier '" << m_identifier << "'";
  m_formatted_msg = ss.str();
}
