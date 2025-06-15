#include "driver.h"

#include <unistd.h>

#include <cstdlib>
#include <ctime>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

namespace fs = std::filesystem;

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

void compile_tokens(const std::string& filename, std::ostream& out) {
  try {
    Lexer lexer;
    const std::vector<Token>& tokens = lexer.tokenize(filename);

    print_tokens(tokens, out);
  } catch (const std::exception& e) {
    std::cerr << "Tokenization error: " << e.what() << std::endl;
  }
}

void compile_ast(const std::string& filename, std::ostream& out) {
  try {
    Lexer lexer;
    SymTab symtab;
    Parser parser;

    const std::vector<Token>& tokens = lexer.tokenize(filename);
    std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);

    print_ast(ast, out);
  } catch (const std::exception& e) {
    std::cerr << "AST generation error: " << e.what() << std::endl;
  }
}

void compile_symtab(const std::string& filename, std::ostream& out) {
  try {
    Lexer lexer;
    SymTab symtab;
    Parser parser;

    const std::vector<Token>& tokens = lexer.tokenize(filename);
    parser.parse_program(&symtab, tokens);

    symtab.print(out);
  } catch (const std::exception& e) {
    std::cerr << "Symbol table error: " << e.what() << std::endl;
  }
}

void compile_ir(const std::string& filename, std::ostream& out) {
  try {
    Lexer lexer;
    SymTab symtab;
    Parser parser;
    TypeChecker type_checker;
    IrVisitor ir_visitor(&symtab);

    const std::vector<Token>& tokens = lexer.tokenize(filename);
    std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
    type_checker.check_program(&symtab, ast);
    ir_visitor.visit_all(ast);
    const std::vector<IRInstruction>& instrs = ir_visitor.get_instructions();

    print_ir_instructions(instrs, out);
  } catch (const std::exception& e) {
    std::cerr << "IR generation error: " << e.what() << std::endl;
  }
}

void compile_asm(const std::string& filename, std::ostream& out) {
  try {
    Lexer lexer;
    SymTab symtab;
    Parser parser;
    TypeChecker type_checker;
    IrVisitor ir_visitor(&symtab);
    X86_64CodeGenerator gen;

    const std::vector<Token>& tokens = lexer.tokenize(filename);
    std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
    type_checker.check_program(&symtab, ast);
    ir_visitor.visit_all(ast);
    const std::vector<IRInstruction>& instrs = ir_visitor.get_instructions();

    std::string asm_code = gen.generate(instrs, ir_visitor.is_main_defined());

    out << asm_code;
  } catch (const std::exception& e) {
    std::cerr << "Assembly generation error: " << e.what() << std::endl;
  }
}

void compile_exe(const std::string& filename, const std::string& out_exe) {
  try {
    Lexer lexer;
    SymTab symtab;
    Parser parser;
    TypeChecker type_checker;
    IrVisitor ir_visitor(&symtab);
    X86_64CodeGenerator gen;

    const std::vector<Token>& tokens = lexer.tokenize(filename);
    std::vector<AstPtr> ast = parser.parse_program(&symtab, tokens);
    type_checker.check_program(&symtab, ast);
    ir_visitor.visit_all(ast);
    const std::vector<IRInstruction>& instrs = ir_visitor.get_instructions();

    std::string asm_code = gen.generate(instrs, ir_visitor.is_main_defined());
    assemble_and_link(asm_code, out_exe);
  } catch (const std::exception& e) {
    std::cerr << e.what() << std::endl;
  }
}
