#ifndef LEXER_SPAN_H
#define LEXER_SPAN_H

#include <cstddef>
#include <ostream>

class Span {
public:
  size_t row;
  size_t start_col;
  size_t end_col; // the span is exclusive of the end col
  Span() : row(0), start_col(0), end_col(0) {}
  Span(size_t row, size_t start_col, size_t end_col)
      : row(row), start_col(start_col), end_col(end_col) {}

  inline size_t len() const { return end_col - start_col; }

  inline std::string to_string() const {
    return "(row=" + std::to_string(row) +
           ", col=" + std::to_string(start_col) + "-" +
           std::to_string(end_col) + ")";
  }

  inline friend std::ostream& operator<<(std::ostream& out, const Span& span) {
    out << span.to_string();
    return out;
  }
};

#endif // LEXER_SPAN_H
