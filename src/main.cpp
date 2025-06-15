#include <fstream>
#include <iostream>
#include <string>
#include <unordered_set>

#define drive_print(func, input, output) \
  if (output.empty()) {                  \
    func(input, std::cout);              \
  } else {                               \
    std::ofstream out(output);           \
    func(input, out);                    \
    out.close();                         \
  }

#include "driver.h"

static void usage() {
  std::cerr << "Usage: ./a.out <input>\n"
            << "       [--tokens [filename]]\n"
            << "       [--ast [filename]]\n"
            << "       [--symtab [filename]]\n"
            << "       [--ir [filename]]\n"
            << "       [--asm [filename]]\n"
            << "       [--exe [filename]]\n";
  exit(1);
}

static void drive(const std::string& arg, const std::string& input,
                  const std::string& output) {
  if (arg == "--tokens") {
    drive_print(compile_tokens, input, output);
  } else if (arg == "--ast") {
    drive_print(compile_ast, input, output);
  } else if (arg == "--symtab") {
    drive_print(compile_symtab, input, output);
  } else if (arg == "--ir") {
    drive_print(compile_ir, input, output);
  } else if (arg == "--asm") {
    drive_print(compile_asm, input, output);
  } else if (arg == "--exe") {
    if (output.empty()) throw std::runtime_error("Exe output file must exist");
    compile_exe(input, output);
  }
}

int main(int argc, char** argv) {
  if (argc < 2) {
    usage();
  }
  std::string input = argv[1];
  std::unordered_set<std::string> opts = {"--tokens", "--ast", "--symtab",
                                          "--ir",     "--asm", "--exe"};
  for (int i = 2; i < argc;) {
    std::string arg = argv[i];
    if (opts.find(arg) != opts.end()) {
      if (i + 1 < argc && argv[i + 1][0] != '-') {
        drive(arg, input, argv[i + 1]);
        i += 2;
      } else {
        drive(arg, input, "");
        i += 1;
      }
    } else {
      std::cerr << "Unknown option: " << arg << std::endl;
      return 1;
    }
  }
  return 0;
}
