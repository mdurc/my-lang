# Lexer
- [ ] Comments
    - `//` for single line
    - `/* ... */` for multi-line
- [ ] Number types
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
- [ ] Spans for every token
- [ ] Identifiers (alphanumeric and `_` starting with non-number)
- [ ] Keyword recognition
- [ ] Panic recovery upon unknown tokens

# Symbol Table
- [ ] Holds declarations of variable/function names
- [ ] Holds the scope depth of the variable
- [ ] Holds whether or not this variable has been assigned/initialized

# Parser
- [ ] Recursive descent function structure based on grammar
- [ ] Add identifiers to a symbol table
- [ ] Create AST that will be passed to the type checker
- [ ] Panic mode recovery for multiple error reporting without cascading

# AST
- [ ] Tree of operation nodes
- [ ] Each node contains its original token so that the span is maintained for the LSP and error detection
- [ ] Each node contains its scope depth so that the LSP can retrieve it to locate the correct declaration within the symbol table
- [ ] Each node has a resolved state that is evaluated by the type checker to enforce type correctness
- Nodes:
    - [ ] Literal Data
    - [ ] Identifier
    - [ ] Variable Declaration
    - [ ] Assignment
    - [ ] Block
    - [ ] Function Declaration
        - [ ] Function Parameter
    - [ ] Function Call
    - [ ] Binary Operation
    - [ ] Unary Operation
    - [ ] If-then-else statement
        - [ ] Break/Continue statements
    - [ ] Switch Statement
        - [ ] Case clause
    - [ ] For statement
    - [ ] While statement
    - [ ] Struct Declaration
        - [ ] Field Declaration
        - [ ] Struct Literal Creation
        - [ ] Struct Member Access
    - [ ] New Statement (allocation)
        - [ ] Direct creation of type and value `new T(value)`
        - [ ] Array allocation of type `new T[size]`
    - [ ] Free Statement (deallocation)
        - [ ] Single block deletion `free ptr`
        - [ ] Array block deletion `free[] ptr`
    - [ ] Array Declaration (fixed size)
    - [ ] List Declaration (dynamic list)
    - [ ] Clock Declaration
    - [ ] Throwable Errors

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

- Parameter Ownership Modifiers:
```
func foo(mut x: i32, read y: i32, take z: i32) {
    // return; // empty return is valid for u0 return type
}
```
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
