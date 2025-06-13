extern exit, string_length, print_string, print_char
extern print_newline, print_uint, print_int
extern read_char, read_word, parse_uint
extern parse_int, string_equals, string_copy

default rel
global _start
section .text
_start:
	push rbp
	mov rbp, rsp
	mov r10, 10
	mov DWORD[global_vars + 4], r10d
	call _bar
	mov rsp, rbp ; restore stack
	pop rbp
	xor rdi, rdi
	call exit
_foo:
	push rbp
	mov rbp, rsp
	sub rsp, 16
	push r12
	push r13
	push r14
	mov r10, 20
	mov DWORD[rbp - 4], r10d
	mov edi, DWORD[rbp - 4]
	call print_int
	mov r11, LC0
	mov rdi, r11
	call print_string
	mov r12, 30
	mov DWORD[rbp - 8], r12d
	mov edi, DWORD[rbp - 8]
	call print_int
	mov r13, LC0
	mov rdi, r13
	call print_string
	mov edi, DWORD[rbp - 4]
	call print_int
	mov r14, LC0
	mov rdi, r14
	call print_string
	pop r14
	pop r13
	pop r12
	xor rax, rax ; void return
	mov rsp, rbp ; restore stack
	pop rbp
	ret
_bar:
	push rbp
	mov rbp, rsp
	call _foo
	mov edi, DWORD[global_vars + 4]
	call print_int
	mov r10, LC0
	mov rdi, r10
	call print_string
	xor rax, rax ; void return
	mov rsp, rbp ; restore stack
	pop rbp
	ret

section .data
LC0:
	db 10, 0
section .bss
	global_vars resb 16
