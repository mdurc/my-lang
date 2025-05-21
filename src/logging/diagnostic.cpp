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

// TypeMismatchError
TypeMismatchError::TypeMismatchError(const Span& span,
                                     const std::string& expected_type,
                                     const std::string& got_type)
    : Diagnostic(DiagType::ERROR, span),
      m_expected_type(expected_type),
      m_got_type(got_type) {}

void TypeMismatchError::generate_message() {
  std::stringstream ss;
  ss << format_type() << " " << m_span << ": Type mismatch: Expected '"
     << m_expected_type << "', got '" << m_got_type << "'";
  m_msg = ss.str();
}

// VariableNotFoundError
VariableNotFoundError::VariableNotFoundError(const Span& span,
                                             const std::string& identifier)
    : Diagnostic(DiagType::ERROR, span), m_identifier(identifier) {}

void VariableNotFoundError::generate_message() {
  std::stringstream ss;
  ss << format_type() << " " << m_span << ": VariableNotFound: identifier '"
     << m_identifier << "'";
  m_msg = ss.str();
}

// TypeNotFoundError
TypeNotFoundError::TypeNotFoundError(const Span& span, const std::string& type)
    : Diagnostic(DiagType::ERROR, span), m_type(type) {}

void TypeNotFoundError::generate_message() {
  std::stringstream ss;
  ss << format_type() << " " << m_span << ": TypeNotFound: identifier '"
     << m_type << "'";
  m_msg = ss.str();
}
