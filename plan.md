# Lexer
- [x] Comments
    - `//` for single line
    - `/* ... */` for multi-line
- [x] Number types
    - `u0`: null, zero size type, used as void
    - `u8`: unsigned 8-bits
    - `u16`: unsigned 16-bits
    - `u32`: unsigned 32-bits
    - `u64`: unsigned 64-bits
    - `i8`: signed 8-bits
    - `i16`: signed 16-bits
    - `i32`: signed 32-bits
    - `i64`: signed 64-bits
    - `f64`: 64-bit float (no 32-bit, 64 seems to be standard)
- [x] Spans for every token
- [x] Identifiers (alphanumeric and `_` starting with non-number)
- [x] Keyword recognition
- [x] Panic recovery upon unknown tokens

# Symbol Table
- [x] Holds declarations of Named types
    - Though currently auto-initializes all of the primitive types preemptively
- [x] Scopes are composed of:
    - Parent scope id
    - List of types that were defined in this scope, where if a type exists within its parent scope, it would not exist here, as it was defined within the parent scope.
    - List of variables defined in this scope.
- [x] Symbol table is a collection of scopes that are indexed by scope id which is an integer.

# Type System
- Each type has an associated scope id and data
- [x] Named Types
    - Primitives
    - User defined types: structs
- [x] Function Types
    - `func(<param types>)-><return type>`
    - Where multiple functions can share a function type as the same method family.
- [x] Pointer Types
- [x] Variables: name, borrow state modifier, type, scope\_id

# Parser
- [x] Recursive descent function structure based on grammar
- [x] Add identifiers to a symbol table
- [x] Create AST that will be passed to the type checker
- [x] Panic mode recovery for multiple error reporting without cascading

# AST
- [x] Tree of operation nodes
- [x] Each node contains its original token so that the span is maintained for the LSP and error detection
- [x] Each node contains its scope depth so that the LSP can retrieve it to locate the correct declaration within the symbol table
- [x] Each node has a resolved state that is evaluated by the type checker to enforce type correctness
- Nodes:
    - [x] Literal Data
    - [x] Identifier
    - [x] Variable Declaration
    - [x] Assignment
    - [x] Block
    - [x] Function Declaration
        - [x] Function Parameter
    - [x] Function Call
    - [x] Binary Operation
    - [x] Unary Operation
    - [x] If-then-else statement
    - [x] Switch Statement
        - [x] Case clause
    - [x] For statement
        - [x] Break/Continue statements
    - [x] While statement
    - [x] Struct Declaration
        - [x] Field Declaration
        - [x] Struct Literal Creation
        - [x] Struct Member Access
    - [x] New Statement (allocation)
        - [x] Direct creation of type and value `new<T>(value)`
        - [x] Array allocation of type `new<T>[size]`
    - [x] Free Statement (deallocation)
        - [x] Single block deletion `free ptr`
        - [x] Array block deletion `free[] ptr`
    - [ ] Array Declaration (fixed size)
    - [ ] List Declaration (dynamic list)
    - [ ] Clock Declaration
    - [ ] Throwable Errors

# IR (three address code)
- [x] Three address code of standard operations (loops, conditionals, declarations, functions decls, function calls, asm blocks)
- [x] Components:
    - Register
    - ParameterSlot (for the codegen to know the current index of the parameters so that if any overflow into the stack, we can identify the proper offsets)
    - Variable (includes size of the variable in bytes)
    - Immediate (includes size of the immediate in bytes)
    - Label
- [x] Instruction Component:
    - OpCode, optional result, list of source operands, size of the common size of the operands in this operation.

# x86_64 Code Generation
- [x] Converts the IR into `x86_64` asm
- [x] I try to stick to the asm conventions:
    - Arguments go into arg registers unless the stack is needed for overflow
    - Functions save callee-saved registers, use the `rbp` base ptr for variable allocations on the stack
    - Exit from `_start`

# Post
- [ ] Tree sitter highlighting
- [ ] LSP: Diagnostic, Goto-Definition, Hovering over identifiers and literals

# Syntax

#### Variables
- Immutable by default
- Mutability is bound to the variable, not the type
```
mut x: i32;          // Mutable, uninitialized

mut y: i32 = 3;      // Mutable, explicit type + initializer
z: i32 = 5;          // Immutable, explicit type + initializer

mut a := 10;         // Mutable, inferred type
b := "hello";        // Immutable, inferred type
```

- Declaration vs Assignment
```
mut x := 3; // inferred declaration by using :=
mut y : i32 = 2; // explicit declaration using : T =

x = 13; // assignment using = (we cannot use := because it will re-declare the variable)
```

#### Ownership and Lifetime
- Variables are allocated and deallocated by the compiler
- Variables are deallocated when they go out of scope and have no more references attached to them
    - For now, we will only allow one reference to anything at a time, no reference counting
- Variables must outlive what they are referencing
- Thus, if you do not use Pointers, the code is guaranteed to be memory safe in terms of:
    - No double free
    - No dangling pointers
    - No leaks

#### Control Flow
```
if (condition) {
} else {
}

for (mut i := 0; i < 10; i = i+1) {
    if (i != 5) {
        continue;
    }
    break;
}

while (1) {
}

mut x : i32 = 5;
switch (x) {
    case 3: {
        print x;
    }
}

// block
{ 
    mut x := 3;
}
```

#### Functions
- Explicit and implicit returns via one named return variable:
```
func foo () returns (z: i32) {
    z = 18; // note that we cannot re-declare z using := operator, as it has already been typed as i32
}
func bar () returns (z: i32) {
    return 5; // explicit returns
}
```

- First class functions (note that the function definition syntax above is the
  only way to declare a function 'literal', i.e. you cannot do `x : func = (){}`
  and must instead do `func x (){}`)
    - Thus there is no support for anonymous functions
```
// First class usage (assignment):
temp : func()->u0 = foo;
test := foo;
```

- Parameter Ownership Modifiers:
```
func foo(mut x: i32, read y: i32, take z: i32) {
    // return; // empty return is valid for u0 return type
}
```
- `foo` has a type of `func(mut i32, read i32, take i32)->u0`
- `x` is a mutable reference (not a copy)
    - Argument passed for `x` must be mutable (we cannot make an immutable variable mutable via passing it to a mutable parameter)
- `y` is an immutable reference
    - This is the default
- `z` is its own variable that is owned within this function
    - This will be either a transfer ownership from the argument (if it is a literal or new value then this can happen very easily, otherwise the argument passed will become uninitialized), a copy of the argument (if copiable)
    - `foo(x, y, give z)` will do transfer of ownership and z will be uninitialized
    - `foo(x, y, z)` will do a copy of `z`
    - `foo(x, y, 3)` will do a transfer of ownership, though really just an assignment


#### Structs
```
struct Point {
    x: i32,
    y: i32,

    func is_origin() returns (r: u8) {
        return x == 0 and y == 0;
    }
}

mut origin := Point(x = 0, y = 0);

mut x_o : i32 = origin.x;
mut y_o : i32 = origin.y;
```

#### Pointers
- We will only have raw pointers
```
// Address Of
mut x : i32 = 3;
mut px : ptr<imm i32> = &x; // mutable pointer to immutable reference to i32
mut px : ptr<mut i32> = &mut x; // mutable pointer to mutable reference to i32

// Heap Allocations:
px = new<mut i32>[3]; // allocated 12 bytes of data for three i32, uninitialized variables
free[] px; // free array of bytes

px = new<mut i32>(15); // allocated an i32 on the heap with initialized value 15
free px;

// Accessing
px = &x;
print px; // address
print *px; // value
print *(px + 0); // value at px, using pointer arithmetic
```

#### ASM Blocks and Errors
- For inline assembly:
```
asm {
    mov ...
}
```
- For fatal errors:
```
Error(...);
```
