CC = g++-14
CFLAGS = -std=c++17 -Wall -Wextra -g -Isrc
PROGRAM = sunnyc
BUILD_DIR = build

LEXER_SRCS = src/lexer/lexer.cpp \
						 src/lexer/token.cpp
DIAG_SRCS = src/logging/diagnostic.cpp
PARSER_SRC = src/parser/parser.cpp \
						 src/parser/symtab.cpp

SOURCE = $(LEXER_SRCS) $(DIAG_SRCS) $(PARSER_SRC)
PROGRAM_SRCS = src/main.cpp $(SOURCE)

PROGRAM_OBJS = $(patsubst src/%.cpp,$(BUILD_DIR)/%.o,$(PROGRAM_SRCS))

all: $(PROGRAM)

$(PROGRAM): $(PROGRAM_OBJS)
	@$(CC) $(CFLAGS) $^ -o $@

$(BUILD_DIR)/%.o: src/%.cpp
	@mkdir -p $(@D)
	@$(CC) $(CFLAGS) -c $< -o $@

clean:
	@rm -rf $(PROGRAM) $(BUILD_DIR)

.PHONY: all clean tests update_snapshots
