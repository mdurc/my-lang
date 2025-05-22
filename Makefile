CC = g++-14
CFLAGS = -std=c++17 -Wall -Wextra -g -Isrc
PROGRAM = sunnyc
TEST = lexer_tests
BUILD_DIR = build

LEXER_SRCS = src/lexer/lexer.cpp \
						 src/lexer/token.cpp
DIAG_SRCS = src/logging/diagnostic.cpp
PARSER_SRC = src/parser/parser.cpp \
						 src/parser/symtab.cpp \
						 src/parser/types.cpp \
						 src/parser/ast.cpp

SOURCE = $(LEXER_SRCS) $(DIAG_SRCS) $(PARSER_SRC)
PROGRAM_SRCS = src/main.cpp $(SOURCE)
TEST_SRCS = src/testing/lexer_tests.cpp \
						src/testing/test_harness.cpp \
						$(SOURCE)

PROGRAM_OBJS = $(patsubst src/%.cpp,$(BUILD_DIR)/%.o,$(PROGRAM_SRCS))
TEST_OBJS = $(patsubst src/%.cpp,$(BUILD_DIR)/%.o,$(TEST_SRCS))

all: $(PROGRAM) $(TEST)

$(PROGRAM): $(PROGRAM_OBJS)
	@$(CC) $(CFLAGS) $^ -o $@

$(TEST): $(TEST_OBJS)
	@$(CC) $(CFLAGS) $^ -o $@

$(BUILD_DIR)/%.o: src/%.cpp
	@mkdir -p $(@D)
	@$(CC) $(CFLAGS) -c $< -o $@

run_tests: $(TEST)
	./$(TEST)

update_snapshots: $(TEST)
	./$(TEST) --update-snapshots

clean:
	@rm -rf $(PROGRAM) $(TEST) $(BUILD_DIR)

.PHONY: all clean tests update_snapshots
