#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>
#include <unordered_set>

#include "driver.h"

static void usage() {
  std::cerr << "Usage: ./a.out <input>\n"
            << "       [--tokens [filename]]\n"
            << "       [--ast [filename]]\n"
            << "       [--symtab [filename]]\n"
            << "       [--ir [filename]]\n"
            << "       [--asm [filename]]\n"
            << "       [--exe [filename]]\n"
            << "       [--repl]\n";
  exit(1);
}

int main(int argc, char** argv) {
  if (argc < 2) {
    usage();
  }

  std::string input = argv[1];
  if (input == "--repl") {
    drive(input, "", "");
    return 0;
  }
  std::unordered_set<std::string> opts = {"--tokens", "--ast", "--symtab",
                                          "--ir",     "--asm", "--exe",
                                          "--json",   "--repl"};
  for (int i = 2; i < argc;) {
    std::string arg = argv[i];
    if (opts.find(arg) != opts.end()) {
      bool success = false;
      if (i + 1 < argc && argv[i + 1][0] != '-') {
        success = drive(arg, input, argv[i + 1]);
        i += 2;
      } else {
        success = drive(arg, input, "");
        i += 1;
      }

      if (!success) {
        return 1;
      }
    } else {
      std::cerr << "Unknown option: " << arg << std::endl;
      return 1;
    }
  }
  return 0;
}
