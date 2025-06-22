# My Language and its Compiler

This is a technical overview of the compiler and the language it compiles.
- [Grammar](info/grammar.txt)
- [Syntax](info/syntax.md)

## 1. Overview

My language is a statically-typed, ahead-of-time compiled programming language. It is designed with small similarties in syntax to C, rust, mojo. Written in C++ and targets the x86-64 architecture on macOs.

Primary goals:
- **Strong type safety**: Type safety is a big priority, particularly with regards to mutability as well.
- **Low-level**: direct memory manipulation and unsafe pointers are important, and inline assembly seemed really interesting.

## 2. Design

Follows a standard flow of passes until producing the assembly:
1. **Preprocessing**: Handles `#include` and `#define` directives.
2.  **Lexical Analysis (Lexer)**: Converts the source text into a stream of tokens.
3.  **Syntactic Analysis (Parser)**: Builds an Abstract Syntax Tree (AST) from the token stream and populates the symbol table with initial declarations of types and variables.
4.  **Semantic Analysis (Type Checking)**: Traverses the AST to enforce type rules, resolve expression types, and check for semantic errors such as VariableNotFound.
5.  **Intermediate Representation (IR) Generation**: Translates the annotated AST into a three-address code IR format for the code generator.
6.  **Code Generation**: Converts the IR into target-specific (x86-64 as of now) assembly code. Utilizes a runtime asm library I wrote for help with more complex/lengthy operations such as `string_copy` or `parse_int`.
7.  **Assembly & Linking**: Uses external tools (`nasm`, `ld`) to assemble the generated code and link it with the language's runtime library to create an executable.

Each stage exists within the `src/` directory, where `src/driver.cpp` is the driver for all of the CLI options for the compiler.

## 3. Preprocessing

The preprocessor runs just before the driver of the lexer and handles two main directives:

-   `#define IDENTIFIER value`: Implements a simple direct text substitution. It replaces all occurrences of `IDENTIFIER` as a whole word with `value`.
-   `#include "path/to/file.sn"`: Inlines the content of another file. The preprocessor tracks included files to detect and report circular dependencies.

## 4. Lexical Analysis (Lexer)

-   **Functionality**: It scans the preprocessed source code and converts it into a sequence of `Token` objects.
-   **Comments**: It recognizes and discards single-line (`// ...`) and multi-line (`/* ... */`) comments.
-   **Tokens**: It identifies keywords (`func`, `if`, etc.), identifiers, operators (`+`, `*`, `==`, etc.), and literals.
-   **Literals**:
    -   **Integer**: `i64`
    -   **Float**: `f64`
    -   **String**: UTF-8 strings enclosed in double quotes (e.g., `"hello"`). Supports `\n`, `\t`, `\"`, `\\` escape sequences.
    -   **Boolean**: `true` and `false`.
    -   **Null**: `null`.
-   **Location Tracking**: Each token stores a `Span` (row, start column, end column) for precise error reporting and LSP information.


## 6. Syntactic Analysis (Parser)

Recursive descent parser, heavily mirroring the grammar in `info/grammar.txt`, that consumes the token stream and builds the AST

-   **AST Structure**: Each node contains a pointer to its source `Token` for error reporting and its `scope_id`.
    - Each node contains its scope depth so that the LSP can retrieve it to locate the correct declaration within the symbol table.
-   **Symbol Table Interaction**: During parsing, when a declaration (`func`, `struct`, variable) is encountered, it is immediately added to the current scope in the symbol table. We thus also check for TypeNotFound errors when parsing types.
-   **Error Recovery**: The parser implements panic mode. On a syntax error, it reports the error and then calls `synchronize()` to advance the token stream until it finds a "synchronization point". This allows it to report multiple independent errors in a single pass.

## 7. Symbol Table and Scoping

-   **Structure**: It is implemented as a `std::vector<Scope>`, where the vector index is the `scope_id`. Each `Scope` object contains a hash map for symbols and a pointer to its parent scope's ID, forming a parent-pointer tree of scopes.
-   **Symbols**: It stores `Variable` and `Type` symbols. `Variable` symbols contain their name, type, borrow state, and scope.
-   **Scoping Rules**:
    -   **Global Scope (ID 0)**: Contains built-in types and all top-level function and struct declarations.
    -   **Function Scope**: When parsing a function, a new scope is created for its parameters and body.
    -   **Block Scope**: Any block enclosed in `{ ... }` creates a new nested scope. This allows for variable shadowing.
    -   **Loop Scopes**: `for` loops create a new scope that encompasses the initializer, condition, iteration, and body. This means a variable declared in the `for` initializer is only visible within the loop.
-   **Lookup**: When looking up a symbol, the search starts at the current scope and traverses up the parent chain to the global scope until the symbol is found, thus supporting shadowing of variables as well.

## 8. Semantic Analysis (Type Checker)

Utilizes the visitor design pattern to traverse the AST and enforce the type rules.

-   **Type Resolution**: It determines the type of every expression node and annotates the AST by setting the `expr_type` field on each `ExpressionNode`.
-   **Type Inference**: For `:=` declarations, it infers the variable's type from the type of the initializer expression.
-   **Type Compatibility**: It enforces rules for operations:
    -   **Assignment**: The right-hand side's type must be assignable to the left-hand side's type. This allows for implicit casting between numeric types. `ptr<imm T>` can be assigned from `ptr<mut T>`.
    -   **Operators**: Binary operators like `+` are checked for numeric or string operands. Logical operators `and`/`or` require boolean operands.
    -   **Function Calls**: The number and types of arguments must match the function's parameter types.
-   **L-value and Mutability Checking**: Verifies that assignments and mutable borrows are only performed on valid, mutable locations in memory. This prevents modifying immutable variables or r-values.

### 8.1 Type System
Each type has an associated scope id and data

- Named Types: primitives, user-defined types (structs)
- Function Types: `func(modifier:type)->type`
    - Where multiple functions can share a function type as the same method family.
- Pointer Types
- Variables: name, borrow state modifier, type, `scope_id`

## 9. Intermediate Representation (IR)

-   **Format**: Three-Address Code (TAC)
-   **Instructions**: `IRInstruction` is the core data structure. It has an `IROpCode`, an optional destination operand, a list of source operands, and a size.
-   **Operands**: `IROperand` is a `std::variant` for:
    -   `IR_Register`: A temporary, virtual register (`_tN`).
    -   `IR_Variable`: A named variable from the source.
        - Includes size of the variable in bytes, and if it is actually a function declaration due to first class functions.
    -   `IR_Immediate`: A constant value.
        - Includes size of the immediate in bytes
    -   `IR_Label`: A target for control flow jumps.
    -   `IR_ParameterSlot`: Represents an incoming function argument. It is for the codegen to know the current index of the parameters so that if any overflow into the stack, we can identify the proper offsets.
-   **Generation**: The `IrVisitor` walks the AST. It translates expressions into sequences of IR instructions that compute the result into a temporary register. It converts control flow structures like `if` and `while` into conditional jumps (`IF_Z`) and labels.

## 10. Code Generation (x86-64)

-   **Target**: The generator produces NASM-syntax assembly for macOS, following the System V AMD64 ABI.
-   **Register Allocation**:
    -   Greedy selection of registers, spilling to the stack if necessary. A pool of general-purpose registers (`r10`, `r11`, `rbx`, etc) is reserved for mapping to IR temporary registers.
    -   **Spilling**: If all physical registers are in use, the code generator "spills" an existing register's value to the stack to free it up. It tracks spilled locations and reloads them when needed.
-   **Stack Frame Management**:
    -   Each function has a standard stack frame set up with `rbp` as the frame pointer.
    -   The function prologue saves the previous `rbp`, sets the new `rbp`, and subtracts from `rsp` to allocate space for all local variables, parameters passed on the stack, and spilled registers.
    -   The function epilogue restores `rsp` and `rbp` before returning.
-   **Calling Convention**:
    -   The first six integer/pointer arguments are passed in `rdi`, `rsi`, `rdx`, `rcx`, `r8`, `r9`. Subsequent arguments are pushed onto the stack in reverse order.
    -   The return value is placed in `rax`.
    -   The generator saves and restores any caller-saved registers that are live across a `call` instruction. It also saves and restores any used callee-saved registers in the function prologue/epilogue.
-   **Entry Point**: The final executable's entry point is `_start`. This label marks the beginning of the top-level code. If a `main` function is defined, `_start` will execute all top-level statements and then `call main`. The return value of `main` is used as the program's exit code. If no `main` exists, the program exits after the top-level code.

## 11. Runtime Library

-   **I/O**: `print_string`, `print_int`, `print_char`, `read_word`, etc.
-   **Memory Management**: `malloc` and `free` are wrappers around the `mmap` and `munmap` syscalls.
-   **String Utilities**: `string_length`, `string_equals`, `string_copy`.
-   **Other**: `memcpy`, `clrscr`.

## 12. Driver

-   It parses command-line arguments to control which stage of the compilation to run and where to output the result.
-   It orchestrates the pipeline by creating and running the Preprocessor, Lexer, Parser, TypeChecker, IR Visitor, and Code Generator in sequence.
-   For producing an executable, it writes the generated assembly to a temporary file, invokes `nasm` to assemble it into an object file, and then invokes `ld` to link the object file with the runtime library.

## Other
- Tree sitter highlighting
- LSP: Diagnostic, Goto-Definition, Hovering over identifiers and literals
- [CLI](cli.py) can be installed to local bin with `make install` where you can then use it as `mcompiler_cli --help`
