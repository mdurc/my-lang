#ifndef PREPROCESSOR_PREPROCESSOR_H
#define PREPROCESSOR_PREPROCESSOR_H

#include <fstream>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "../logging/logger.h"

struct PreprocessedFile {
  std::string content;
  std::string filename;
  std::vector<std::string> included_files;
};

class Preprocessor {
public:
  Preprocessor(Logger* logger) : m_logger(logger) {}

  PreprocessedFile preprocess_file(const std::string& filename);

private:
  Logger* m_logger;

  std::unordered_map<std::string, std::string> m_macros;

  std::vector<std::string> m_include_paths;

  // for checking for circular inclusions
  std::unordered_set<std::string> m_processing_files;

  // process a single file
  std::string process_file_content(const std::string& filename,
                                   const std::string& content);

  // handle different preprocessor directives
  void handle_define_directive(const std::string& line);
  void handle_include_directive(const std::string& line,
                                std::stringstream& output);

  // macro expansion
  std::string expand_macros(const std::string& content);

  std::string find_include_file(const std::string& include_name);
  std::string read_file_content(const std::string& filename);
  std::vector<std::string> split_lines(const std::string& content);
  std::string trim_whitespace(const std::string& str);
};

#endif // PREPROCESSOR_PREPROCESSOR_H
