#ifndef LOGGING_LOGGER_H
#define LOGGING_LOGGER_H

#include <vector>

#include "diagnostic.h"

class Logger {
public:
  Logger() = default;
  void report(Diagnostic err) {
    switch (err.get_type()) {
      case DiagType::FATAL_ERROR:
      case DiagType::ERROR: m_errors.push_back(std::move(err)); break;
      case DiagType::WARNING:
      case DiagType::HINT: m_warnings.push_back(std::move(err)); break;
    }
  }

  const std::vector<Diagnostic>& get_errors() const { return m_errors; }
  const std::vector<Diagnostic>& get_warnings() const { return m_warnings; }
  size_t num_errors() const { return m_errors.size(); }
  size_t num_warnings() const { return m_warnings.size(); }

  void clear() {
    m_errors.clear();
    m_warnings.clear();
  }

  bool output_diagnostics() const {
    if (num_errors() == 0) {
      for (const Diagnostic& d : m_warnings) {
        std::cerr << d.what() << std::endl;
      }
      return false;
    }

    for (const Diagnostic& d : m_warnings) {
      std::cerr << d.what() << std::endl;
    }

    for (const Diagnostic& d : m_errors) {
      std::cerr << d.what() << std::endl;
    }

    return true;
  }

private:
  std::vector<Diagnostic> m_errors;   // fatal errors and errors
  std::vector<Diagnostic> m_warnings; // warnings and hints
};

#endif // LOGGING_LOGGER_H
