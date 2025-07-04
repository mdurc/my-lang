#include "driver.h"

#include <unistd.h>

#include <cstdlib>
#include <ctime>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include "json_export/json_exporter.h"

#define _check_diags()                                \
  do {                                                \
    if (logger.num_fatals() || logger.num_errors()) { \
      throw FatalError("");                           \
    } else if (logger.num_warnings()) {               \
      std::cerr << logger.get_diagnostic_str();       \
    }                                                 \
  } while (0)

#define drive_print(func, input, output) \
  if (output.empty()) {                  \
    func(input, std::cout);              \
  } else {                               \
    std::ofstream out(output);           \
    bool _ret = func(input, out);        \
    out.close();                         \
    return _ret;                         \
  }

namespace fs = std::filesystem;

bool drive(const std::string& arg, const std::string& input,
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
    return compile_exe(input, output);
  } else if (arg == "--json") {
    drive_print(compile_json, input, output);
  } else if (arg == "--repl") {
    run_repl();
  }
  return true;
}

bool compile_tokens(const std::string& filename, std::ostream& out) {
  Logger logger;
  Lexer lexer(&logger);
  try {
    std::vector<Token> tokens = lexer.tokenize_file(filename);
    _check_diags();
    print_tokens(tokens, out);
  } catch (const FatalError&) {
    std::cerr << "Error\n" << logger.get_diagnostic_str();
    return false;
  }
  return true;
}

bool compile_ast(const std::string& filename, std::ostream& out) {
  Logger logger;
  Lexer lexer(&logger);
  SymTab symtab;
  Parser parser(&logger);
  try {
    std::vector<Token> tokens = lexer.tokenize_file(filename);
    _check_diags();
    std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
    _check_diags();
    print_ast(ast, out);
  } catch (const FatalError&) {
    std::cerr << "Error\n" << logger.get_diagnostic_str();
    return false;
  }
  return true;
}

bool compile_symtab(const std::string& filename, std::ostream& out) {
  Logger logger;
  Lexer lexer(&logger);
  SymTab symtab;
  Parser parser(&logger);
  try {
    std::vector<Token> tokens = lexer.tokenize_file(filename);
    _check_diags();
    parser.parse_program(&symtab, tokens);
    _check_diags();
    symtab.print(out);
  } catch (const FatalError&) {
    std::cerr << "Error\n" << logger.get_diagnostic_str();
    return false;
  }
  return true;
}

bool compile_ir(const std::string& filename, std::ostream& out) {
  Logger logger;
  Lexer lexer(&logger);
  SymTab symtab;
  Parser parser(&logger);
  TypeChecker type_checker(&logger);
  IrVisitor ir_visitor(&symtab, &logger);
  try {
    std::vector<Token> tokens = lexer.tokenize_file(filename);
    _check_diags();
    std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
    _check_diags();
    type_checker.check_program(&symtab, ast);
    _check_diags();
    ir_visitor.visit_all(ast);
    _check_diags();
    const std::vector<IRInstruction>& instrs = ir_visitor.get_instructions();
    print_ir_instructions(instrs, out);
  } catch (const FatalError&) {
    std::cerr << "Error\n" << logger.get_diagnostic_str();
    return false;
  }
  return true;
}

bool compile_asm(const std::string& filename, std::ostream& out) {
  Logger logger;
  Lexer lexer(&logger);
  SymTab symtab;
  Parser parser(&logger);
  TypeChecker type_checker(&logger);
  IrVisitor ir_visitor(&symtab, &logger);
  X86_64CodeGenerator gen(&logger);
  try {
    std::vector<Token> tokens = lexer.tokenize_file(filename);
    _check_diags();
    std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
    _check_diags();
    type_checker.check_program(&symtab, ast);
    _check_diags();
    ir_visitor.visit_all(ast);
    _check_diags();
    const std::vector<IRInstruction>& instrs = ir_visitor.get_instructions();
    std::string asm_code = gen.generate(instrs, ir_visitor.is_main_defined());
    _check_diags();
    out << asm_code;
  } catch (const FatalError&) {
    std::cerr << "Error\n" << logger.get_diagnostic_str();
    return false;
  }
  return true;
}

static void assemble_and_link(const std::string& asm_code,
                              const std::string& output_exe) {
  fs::path temp_dir = fs::temp_directory_path();
  std::string temp_prefix = "mycomp_" + std::to_string(getpid()) + "_" +
                            std::to_string(std::time(nullptr)) + "_";
  fs::path asm_path = temp_dir / (temp_prefix + "out.asm");
  fs::path obj_path = temp_dir / (temp_prefix + "out.o");
  fs::path exe_path = output_exe;

  std::ofstream asm_out(asm_path);
  asm_out << asm_code;
  asm_out.close();

  std::string runtime_obj = "/usr/local/bin/mycompiler_lib/runtime.o";

  std::string assemble_cmd =
      "nasm -f macho64 " + asm_path.string() + " -o " + obj_path.string();
  if (std::system(assemble_cmd.c_str()) != 0) {
    fs::remove(asm_path);
    fs::remove(obj_path);
    throw std::runtime_error("Assembly failed");
  }
  std::string link_cmd =
      "ld " + obj_path.string() + " " + runtime_obj + " -o " +
      exe_path.string() +
      " -macos_version_min 10.13 -e _start -lSystem -no_pie" +
      " -syslibroot $(xcrun --sdk macosx --show-sdk-path)";
  if (std::system(link_cmd.c_str()) != 0) {
    fs::remove(asm_path);
    fs::remove(obj_path);
    throw std::runtime_error("Linking failed");
  }

  fs::remove(asm_path);
  fs::remove(obj_path);
}

bool compile_exe(const std::string& filename, const std::string& out_exe) {
  Logger logger;
  Lexer lexer(&logger);
  SymTab symtab;
  Parser parser(&logger);
  TypeChecker type_checker(&logger);
  IrVisitor ir_visitor(&symtab, &logger);
  X86_64CodeGenerator gen(&logger);
  try {
    std::vector<Token> tokens = lexer.tokenize_file(filename);
    _check_diags();
    std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
    _check_diags();
    type_checker.check_program(&symtab, ast);
    _check_diags();
    ir_visitor.visit_all(ast);
    _check_diags();
    const std::vector<IRInstruction>& instrs = ir_visitor.get_instructions();
    std::string asm_code = gen.generate(instrs, ir_visitor.is_main_defined());
    _check_diags();
    assemble_and_link(asm_code, out_exe);
  } catch (const FatalError&) {
    std::cerr << "Error\n" << logger.get_diagnostic_str();
    return false;
  }
  return true;
}

bool compile_json(const std::string& filename, std::ostream& out) {
  Logger logger;
  Lexer lexer(&logger);
  SymTab symtab;
  Parser parser(&logger);
  TypeChecker type_checker(&logger);
  std::vector<Token> tokens;
  std::vector<AstPtr> ast;
  IrVisitor ir_visitor(&symtab, &logger);
  X86_64CodeGenerator gen(&logger);
  try {
    tokens = lexer.tokenize_file(filename);
    ast = parser.parse_program(&symtab, tokens);
    type_checker.check_program(&symtab, ast);
    ir_visitor.visit_all(ast);
    const std::vector<IRInstruction>& instrs = ir_visitor.get_instructions();
    std::string asm_code = gen.generate(instrs, ir_visitor.is_main_defined());
    // don't check the diags, leave it to the json output
  } catch (const FatalError&) {
    // do nothing, let the json output the diagnostics
  }
  JsonExporter json_exporter(&symtab, &logger, &ast);
  out << json_exporter.export_to_json() << std::endl;
  return true; // always return true for json output
}

// == Repl implementations: ==
static std::string make_temp(const std::string& source) {
  std::string pid = std::to_string(getpid());
  std::string temp = "/tmp/repl_source_" + pid + ".tmp.sn";
  std::ofstream out(temp);
  out << source;
  out.close();
  return temp;
}

static std::string join_lines(const std::vector<std::string>& lines) {
  std::stringstream ss;
  for (size_t i = 0; i < lines.size(); ++i) {
    if (i > 0) ss << "\n";
    ss << lines[i];
  }
  return ss.str();
}

void run_repl() {
  std::string line;
  std::vector<std::string> lines;
  bool in_multiline = false;

  while (true) {
    if (!in_multiline) {
      std::cout << ">>> ";
    } else {
      std::cout << "... ";
    }

    if (!std::getline(std::cin, line)) {
      break; // EOF
    }

    bool auto_run = line.empty() && !lines.empty() && in_multiline;

    if (line == "exit" || line == "quit") {
      break;
    } else if (line == "help") {
      std::cout << "Commands:" << std::endl;
      std::cout << "  exit, quit - Exit the REPL" << std::endl;
      std::cout << "  help - Show this help" << std::endl;
      std::cout << "  clear - Clear the current input" << std::endl;
      std::cout << "  tokens - Show tokens for current input" << std::endl;
      std::cout << "  ast - Show AST for current input" << std::endl;
      std::cout << "  ir - Show IR for current input" << std::endl;
      std::cout << "  asm - Show assembly for current input" << std::endl;
      std::cout << "  run - Execute the current input" << std::endl;
      std::cout << std::endl;
      std::cout << "Type your code and press Enter twice to execute."
                << std::endl;
      continue;
    } else if (line == "clear") {
      lines.clear();
      in_multiline = false;
      std::cout << "Input cleared." << std::endl;
      continue;
    } else if (line == "tokens") {
      if (lines.empty()) {
        std::cout << "No input to tokenize." << std::endl;
        continue;
      }
      std::string temp_input = make_temp(join_lines(lines));
      drive("--tokens", temp_input, "");
      std::remove(temp_input.c_str());
      continue;
    } else if (line == "ast") {
      if (lines.empty()) {
        std::cout << "No input to parse." << std::endl;
        continue;
      }
      std::string temp_input = make_temp(join_lines(lines));
      drive("--ast", temp_input, "");
      std::remove(temp_input.c_str());
      continue;
    } else if (line == "ir") {
      if (lines.empty()) {
        std::cout << "No input to compile." << std::endl;
        continue;
      }
      std::string temp_input = make_temp(join_lines(lines));
      drive("--ir", temp_input, "");
      std::remove(temp_input.c_str());
      continue;
    } else if (line == "asm") {
      if (lines.empty()) {
        std::cout << "No input to compile." << std::endl;
        continue;
      }
      std::string temp_input = make_temp(join_lines(lines));
      drive("--asm", temp_input, "");
      std::remove(temp_input.c_str());
      continue;
    } else if (line == "run" || auto_run) {
      if (lines.empty()) {
        std::cout << "No input to execute." << std::endl;
        continue;
      }
      std::string pid = std::to_string(getpid());
      std::string temp_exe = "/tmp/repl_temp_" + pid + ".tmp.exe";

      std::string temp_input = make_temp(join_lines(lines));
      bool success = drive("--exe", temp_input, temp_exe);

      if (success) {
        // then we can run the produced executable
        int result = std::system(temp_exe.c_str());
        if (result != 0) {
          std::cout << "Program exited with code " << result << std::endl;
        }
        std::remove(temp_exe.c_str());
      }

      std::remove(temp_input.c_str());

      if (auto_run) {
        lines.clear();
        in_multiline = false;
      }

      continue;
    }

    // check for empty line
    if (!line.empty()) {
      lines.push_back(line);
      in_multiline = true;
      continue;
    }
  }
}
