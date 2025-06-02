#!/bin/bash
nasm -f elf64 src/codegen/runtime/x86_64_lib.asm -o x86_64_lib.o && nasm -f elf64 asm-output.asm -o asm-output.o && ld x86_64_lib.o asm-output.o -o asm-output.exe && ./asm-output.exe
