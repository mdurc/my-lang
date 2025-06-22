#include "preprocessor.h"

#include <filesystem>
#include <fstream>
#include <sstream>

namespace fs = std::filesystem;

PreprocessedFile Preprocessor::preprocess_file(const std::string& filename) {
  m_processing_files.clear();
  m_macros.clear();

  m_include_paths = {"."};

  PreprocessedFile result;

  std::string content = read_file_content(filename);
  if (m_logger->num_fatals() > 0) {
    return result;
  }
  result.content = process_file_content(filename, content);
  result.filename = filename;

  // if errors/warnings were logged, return empty result
  if (m_logger->num_fatals() > 0) {
    return PreprocessedFile{};
  }

  return result;
}

std::string Preprocessor::process_file_content(const std::string& filename,
                                               const std::string& content) {
  if (m_processing_files.find(filename) != m_processing_files.end()) {
    m_logger->report(Error("Circular include detected: " + filename));
    return "";
  }

  m_processing_files.insert(filename);

  std::vector<std::string> lines = split_lines(content);
  std::stringstream output;

  for (size_t i = 0; i < lines.size(); ++i) {
    const std::string& line = lines[i];
    std::string trimmed = trim_whitespace(line);
    if (trimmed.empty()) {
      output << "\n";
      continue;
    }

    if (trimmed[0] != '#') {
      output << expand_macros(line) << "\n";
      continue;
    }

    // else it is a directive that should be processed
    if (trimmed.substr(0, 7) == "#define") {
      output << "\n"; // output a newline as a line placeholder for lexer
      handle_define_directive(trimmed);
    } else if (trimmed.substr(0, 8) == "#include") {
      handle_include_directive(trimmed, output);
    } else {
      // skip other preprocessor directives for now
      m_logger->report(
          Warning("Unsupported preprocessor directive: " + trimmed));
    }
  }

  m_processing_files.erase(filename);
  return output.str();
}

void Preprocessor::handle_define_directive(const std::string& line) {
  // #define macro_name value
  std::istringstream iss(line.substr(8)); // skip '#define '
  std::string macro_name, value;

  iss >> macro_name;
  if (macro_name.empty()) {
    m_logger->report(Error("Expected macro name after #define"));
    return;
  }

  // get the rest of the line as the value
  std::getline(iss, value);
  value = trim_whitespace(value);

  m_macros[macro_name] = value;
}

void Preprocessor::handle_include_directive(const std::string& line,
                                            std::stringstream& output) {
  // #include "filename" or #include <filename>
  std::string include_name;

  char open = line.find('"') != std::string::npos
                  ? '"'
                  : (line.find('<') != std::string::npos ? '<' : '\0');

  if (open != '\0') {
    char close = (open == '"') ? '"' : '>';
    size_t start = line.find(open) + 1;
    size_t end = line.find(close, start);
    if (end != std::string::npos) {
      include_name = line.substr(start, end - start);
    }
  }

  if (include_name.empty()) {
    m_logger->report(Error("Invalid include directive: " + line));
    return;
  }

  std::string include_file = find_include_file(include_name);
  if (include_file.empty()) {
    m_logger->report(Error("Could not find include file: " + include_name));
    return;
  }

  // recursively process the included file
  std::string included_content = read_file_content(include_file);
  output << process_file_content(include_file, included_content);
}

std::string Preprocessor::expand_macros(const std::string& content) {
  std::string result = content;

  // replace macro_name with associated value
  for (const auto& [macro_name, macro_value] : m_macros) {
    size_t len = macro_name.size();
    size_t pos = 0;
    while ((pos = result.find(macro_name, pos)) != std::string::npos) {
      bool is_word_start = (pos == 0) || !isalnum(result[pos - 1]);
      bool is_word_end =
          (pos + len >= result.size()) || !isalnum(result[pos + len]);

      if (is_word_start && is_word_end) {
        result.replace(pos, len, macro_value);
      }
      pos += len;
    }
  }

  return result;
}

std::string Preprocessor::find_include_file(const std::string& include_name) {
  // search in include paths
  for (const std::string& path : m_include_paths) {
    fs::path full_path = fs::path(path) / include_name;
    if (fs::exists(full_path) && fs::is_regular_file(full_path)) {
      return full_path.string();
    }
  }

  return "";
}

std::string Preprocessor::read_file_content(const std::string& filename) {
  std::ifstream file(filename);
  if (!file.is_open()) {
    m_logger->report(Error("Could not open file: " + filename));
    return "";
  }

  std::stringstream buffer;
  buffer << file.rdbuf();
  return buffer.str();
}

std::vector<std::string> Preprocessor::split_lines(const std::string& content) {
  std::vector<std::string> lines;
  std::istringstream iss(content);
  std::string line;

  while (std::getline(iss, line)) {
    lines.push_back(line);
  }

  return lines;
}

std::string Preprocessor::trim_whitespace(const std::string& str) {
  int start = -1, end = -1;
  for (size_t i = 0; i < str.size(); ++i) {
    if (!isspace(str[i])) {
      if (start == -1) start = i;
      end = i;
    }
  }
  return start == -1 ? "" : str.substr(start, end - start + 1);
}
