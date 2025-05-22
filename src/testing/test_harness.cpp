#include "test_harness.h"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

#include "../lexer/lexer.h"

namespace Testing {
std::string read_file_to_string(const std::string& filepath) {
  std::ifstream file(filepath);
  if (!file.is_open()) {
    return "";
  }
  std::stringstream buffer;
  buffer << file.rdbuf();
  return buffer.str();
}

void write_string_to_file(const std::string& filepath,
                          const std::string& content) {
  std::ofstream file(filepath);
  if (file.is_open()) {
    file << content;
  } else {
    std::cerr << "Failed write to snapshot file: " << filepath << std::endl;
  }
}

std::string generate_lexer_output(const std::string& input_filepath) {
  std::stringstream ss;
  Lexer lexer;
  try {
    lexer.tokenize(input_filepath);
    ss << lexer;
  } catch (const std::exception& e) {
    ss << "LEXER_EXCEPTION: " << e.what() << std::endl;
  }
  return ss.str();
}

// Primary Driver Function
bool run_snapshot_test(const std::string& test_name,
                       const std::string& input_filepath,
                       const std::string& snapshot_dir_base,
                       bool update_snapshots) {
  std::cout << "Running test: " << test_name << " ... ";

  std::string actual_output = generate_lexer_output(input_filepath);

  std::filesystem::path input_p(input_filepath);
  std::filesystem::path snapshot_filename = input_p.filename();
  snapshot_filename += ".snap";

  std::filesystem::path snapshot_dir(snapshot_dir_base);
  if (!std::filesystem::exists(snapshot_dir)) {
    std::filesystem::create_directories(snapshot_dir);
  }
  std::filesystem::path snapshot_filepath = snapshot_dir / snapshot_filename;

  if (update_snapshots || !std::filesystem::exists(snapshot_filepath)) {
    std::cout << (update_snapshots ? "UPDATING snapshot" : "CREATING snapshot")
              << " ... ";
    write_string_to_file(snapshot_filepath.string(), actual_output);
    std::cout << "DONE" << std::endl;
    return true;
  }

  std::string expected_output = read_file_to_string(snapshot_filepath.string());

  if (actual_output == expected_output) {
    std::cout << "PASS" << std::endl;
    return true;
  } else {
    std::cout << "FAIL" << std::endl;
    std::cout << "  Expected snapshot: " << snapshot_filepath.string()
              << std::endl;
    return false;
  }
}
} // namespace Testing
