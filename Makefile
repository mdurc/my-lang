CC = g++
CFLAGS = -std=c++17 -Wall -Wextra -g -Isrc
PROGRAM = sunnyc
BUILD_DIR = build

LEXER_SRCS = src/lexer/lexer.cpp \
						 src/lexer/token.cpp
DIAG_SRCS = src/logging/diagnostic.cpp
PARSER_SRC = src/parser/parser.cpp \
						 src/parser/symtab.cpp \
						 src/parser/ast.cpp \
						 src/parser/visitor.cpp \
						 src/parser/types.cpp
CHECKER_SRC = src/checker/typechecker.cpp
CODEGEN = src/codegen/ir/ir_generator.cpp \
					src/codegen/ir/ir_printer.cpp \
					src/codegen/ir/ir_visitor.cpp \
					src/codegen/x86_64/asm.cpp

SOURCE = $(LEXER_SRCS) $(DIAG_SRCS) $(PARSER_SRC) $(CHECKER_SRC) $(CODEGEN)
PROGRAM_SRCS = src/main.cpp $(SOURCE)

PROGRAM_OBJS = $(patsubst src/%.cpp,$(BUILD_DIR)/%.o,$(PROGRAM_SRCS))

all: $(PROGRAM)

$(PROGRAM): $(PROGRAM_OBJS)
	@$(CC) $(CFLAGS) $^ -o $@

$(BUILD_DIR)/%.o: src/%.cpp
	@mkdir -p $(@D)
	@$(CC) $(CFLAGS) -c $< -o $@

clean:
	@rm -rf $(BUILD_DIR)
	@rm -f $(PROGRAM) x86_64_lib.o asm-output.asm asm-output.o asm-output.exe

compile: $(PROGRAM)
	# does not work on mac
	./$(PROGRAM) asm-test.sn > asm-output.asm
	nasm -f elf64 codegen/runtime/x86_64_lib.asm
	nasm -f elf64 asm-output.asm
	ld x86_64_lib.o asm-output.o -o asm-output.exe
	./asm-output.exe

.PHONY: all clean compile
