#ifndef LOGGING_DIAGNOSTIC_H
#define LOGGING_DIAGNOSTIC_H

#include <cstdlib>
#include <exception>
#include <iostream>
#include <sstream>
#include <string>

#include "../lexer/span.h"

enum class DiagType { FATAL_ERROR, ERROR, WARNING, HINT };

// Base Diagnostic Class
class Diagnostic : public std::exception {
public:
  Diagnostic(DiagType type, const std::string& msg = "");
  Diagnostic(DiagType type, const Span& span, const std::string& msg = "");
  virtual ~Diagnostic() = default;

  const char* what() const noexcept override;

  DiagType get_type() const { return m_type; }
  const Span& get_span() const { return m_span; }

protected:
  DiagType m_type;
  Span m_span;
  std::string m_msg;
  std::string m_formatted_msg;

  // should set m_msg to desired message to be used within what()
  std::string format_type() const;
  virtual void generate_message() {
    std::stringstream ss;
    ss << format_type() << " " << m_span << ": " << m_msg;
    m_formatted_msg = ss.str();
  }
};

// == Specific Diagnostics: ==

class FatalError : public Diagnostic {
public:
  FatalError(const std::string& fatal)
      : Diagnostic(DiagType::FATAL_ERROR, fatal) {}
  FatalError(const Span& span, const std::string& fatal)
      : Diagnostic(DiagType::FATAL_ERROR, span, fatal) {}
};

class Error : public Diagnostic {
public:
  Error(const std::string& err) : Diagnostic(DiagType::ERROR, err) {}
  Error(const Span& span, const std::string& err)
      : Diagnostic(DiagType::ERROR, span, err) {}
};

class Warning : public Diagnostic {
public:
  Warning(const std::string& warning)
      : Diagnostic(DiagType::WARNING, warning) {}
  Warning(const Span& span, const std::string& warning)
      : Diagnostic(DiagType::WARNING, span, warning) {}
};

class Hint : public Diagnostic {
public:
  Hint(const std::string& hint) : Diagnostic(DiagType::HINT, hint) {}
  Hint(const Span& span, const std::string& hint)
      : Diagnostic(DiagType::HINT, span, hint) {}
};

class TypeMismatchError : public Diagnostic {
public:
  TypeMismatchError(const Span& span, const std::string& expected_type,
                    const std::string& got_type);

private:
  std::string m_expected_type;
  std::string m_got_type;
  void generate_message() override;
};

class VariableNotFoundError : public Diagnostic {
public:
  VariableNotFoundError(const Span& span, const std::string& identifier);

private:
  std::string m_identifier;
  void generate_message() override;
};

class TypeNotFoundError : public Diagnostic {
public:
  TypeNotFoundError(const Span& span, const std::string& type);

private:
  std::string m_type;
  void generate_message() override;
};

#endif // LOGGING_DIAGNOSTIC_H
