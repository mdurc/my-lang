CC = g++
CFLAGS = -std=c++17 -Wall -Wextra
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
	$(CC) $(CFLAGS) $^ -o $@

$(BUILD_DIR)/%.o: src/%.cpp
	mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(BUILD_DIR)
	rm -f $(PROGRAM) *.o *.exe

update_asm: $(PROGRAM)
	./$(PROGRAM) asm-test.sn --gen > asm-output.asm

compile: $(PROGRAM) update_asm
	nasm -f macho64 src/codegen/runtime/x86_64_lib.asm -o lib.o
	nasm -f macho64 asm-output.asm -o asm-output.asm.o
	ld lib.o asm-output.asm.o -o asm-output.asm.exe \
	-macos_version_min 10.13 \
	-e _start \
	-lSystem \
	-syslibroot $(shell xcrun --sdk macosx --show-sdk-path) \
	-no_pie

run:
	./asm-output.asm.exe

go: compile run

.PHONY: all clean update_asm compile run go
