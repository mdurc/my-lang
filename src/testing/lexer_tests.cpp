#include <filesystem>
#include <iostream>
#include <string>

#include "test_harness.h"

int main(int argc, char* argv[]) {
  bool update_snapshots = false;
  if (argc > 1 && std::string(argv[1]) == "--update-snapshots") {
    update_snapshots = true;
    std::cout << "Snapshot update mode enabled." << std::endl;
  }

  // relative to root
  std::string test_files_dir = "src/testing/samples";
  std::string snapshot_dir = "src/testing/snapshots/lexer";

  if (!std::filesystem::exists(test_files_dir)) {
    std::cerr << "Test directory not found: " << test_files_dir << std::endl;
    return 1;
  }

  int tests_passed = 0;
  int tests_failed = 0;

  for (const auto& entry :
       std::filesystem::directory_iterator(test_files_dir)) {
    if (entry.is_regular_file() && entry.path().extension() == ".sn") {
      std::string test_name = entry.path().stem().string();
      std::string input_filepath = entry.path().string();

      if (Testing::run_snapshot_test(test_name, input_filepath, snapshot_dir,
                                     update_snapshots)) {
        ++tests_passed;
      } else {
        ++tests_failed;
      }
    }
  }

  std::cout << "\n--- Test Summary ---" << std::endl;
  std::cout << "Total tests run: " << (tests_passed + tests_failed)
            << std::endl;
  std::cout << "Passed: " << tests_passed << std::endl;
  std::cout << "Failed: " << tests_failed << std::endl;

  if (tests_failed > 0) {
    std::cout << "\nSome tests failed." << std::endl;
    return 1;
  }

  std::cout << "\nAll lexer tests passed!" << std::endl;
  return 0;
}
