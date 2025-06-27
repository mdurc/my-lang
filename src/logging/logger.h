#ifndef LOGGING_LOGGER_H
#define LOGGING_LOGGER_H

#include <vector>

#include "diagnostic.h"

class Logger {
public:
  Logger() = default;
  void report(Diagnostic err) {
    switch (err.get_type()) {
      case DiagType::FATAL_ERROR: m_fatals.push_back(std::move(err)); break;
      case DiagType::ERROR: m_errors.push_back(std::move(err)); break;
      case DiagType::WARNING:
      case DiagType::HINT: m_warnings.push_back(std::move(err)); break;
    }
  }

  const std::vector<Diagnostic>& get_errors() const { return m_errors; }
  const std::vector<Diagnostic>& get_warnings() const { return m_warnings; }
  const std::vector<Diagnostic>& get_fatals() const { return m_fatals; }
  size_t num_errors() const { return m_errors.size(); }
  size_t num_warnings() const { return m_warnings.size(); }
  size_t num_fatals() const { return m_fatals.size(); }

  void clear() {
    m_errors.clear();
    m_warnings.clear();
  }

  std::string get_diagnostic_str() const {
    std::string out;
    for (const Diagnostic& d : m_warnings) {
      out += d.what();
      out += "\n";
    }

    for (const Diagnostic& d : m_errors) {
      out += d.what();
      out += "\n";
    }

    for (const Diagnostic& d : m_fatals) {
      out += d.what();
      out += "\n";
    }

    return out;
  }

private:
  std::vector<Diagnostic> m_errors; // errors
  std::vector<Diagnostic> m_fatals; // fatal errors (assertions, often internal)
  std::vector<Diagnostic> m_warnings; // warnings and hints
};

#endif // LOGGING_LOGGER_H
