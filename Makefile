CC = g++
MYLIB = mycompiler_lib
PROGRAM = $(MYLIB)/mcompiler
BUILD_DIR = $(MYLIB)/build
CFLAGS = -std=c++17 -Wall -Wextra -g

LEXER_SRCS = src/lexer/lexer.cpp \
						 src/lexer/token.cpp \
						 src/preprocessor/preprocessor.cpp
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
PROGRAM_SRCS = src/driver.cpp src/main.cpp $(SOURCE)

PROGRAM_OBJS = $(patsubst src/%.cpp,$(BUILD_DIR)/%.o,$(PROGRAM_SRCS))

RUNTIME_ASM = src/codegen/runtime/x86_64_lib.asm
RUNTIME_OBJ = $(MYLIB)/runtime.o

all: $(PROGRAM)

$(RUNTIME_OBJ):
	nasm -f macho64 $(RUNTIME_ASM) -o $(RUNTIME_OBJ)

$(PROGRAM): $(PROGRAM_OBJS) $(RUNTIME_OBJ)
	mkdir -p $(MYLIB)
	$(CC) $(CFLAGS) $(PROGRAM_OBJS) -o $@

$(BUILD_DIR)/%.o: src/%.cpp
	mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(MYLIB)
	rm -f asm-test.sn.asm asm-test.sn.exe

# --- Test Workflow ---
TEST_SN_FILE = asm-test.sn
TEST_ASM_FILE = $(TEST_SN_FILE).asm
TEST_EXE_FILE = $(TEST_SN_FILE).exe

update_test_asm: $(PROGRAM)
	mkdir -p $(BUILD_DIR)
	./$(PROGRAM) $(TEST_SN_FILE) --asm $(TEST_ASM_FILE)

compile_test_exe:
	nasm -f macho64 $(TEST_ASM_FILE) -o $(BUILD_DIR)/test_main.o
	ld $(RUNTIME_OBJ) $(BUILD_DIR)/test_main.o -o $(TEST_EXE_FILE) \
	-macos_version_min 10.13 \
	-e _start \
	-lSystem \
	-no_pie \
	-syslibroot $(shell xcrun --sdk macosx --show-sdk-path)

test: update_test_asm compile_test_exe
	./$(TEST_EXE_FILE)

# --- Installation ---
INSTALL_DIR = /usr/local/bin

install: $(PROGRAM) $(RUNTIME_OBJ)
	sudo mkdir -p $(INSTALL_DIR)/$(MYLIB)
	sudo cp $(PROGRAM) $(INSTALL_DIR)/$(PROGRAM)
	sudo cp $(RUNTIME_OBJ) $(INSTALL_DIR)/$(RUNTIME_OBJ)
	sudo cp cli.py $(INSTALL_DIR)/mcompiler_cli
	sudo chmod +x $(INSTALL_DIR)/mcompiler_cli

uninstall:
	sudo rm -f $(INSTALL_DIR)/mcompiler_cli $(INSTALL_DIR)/$(PROGRAM) $(INSTALL_DIR)/$(RUNTIME_OBJ)
	sudo rmdir $(INSTALL_DIR)/$(MYLIB) 2>/dev/null || true

.PHONY: all clean update_test_asm compile_test_exe test install uninstall
