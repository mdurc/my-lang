extern exit, string_length, print_string, print_char
extern print_newline, print_uint, print_int
extern read_char, read_word, parse_uint
extern parse_int, string_equals, string_copy

section .data
.LC0:
	db "", 10, "", 0
.LC1:
	db "hi", 0

global _start
section .text
_start:
	call main
	mov rdi, rax
	call exit
main:
	push rbp
	mov rbp, rsp
	sub rsp, 64
	mov r10, 0
	mov [rbp - 8], r10
	mov r11, 2
	mov rbx, r11
	add rbx, [rbp - 8]
	mov [rbp - 8], rbx
	mov r12, 3
	mov [rbp - 8], r12
	mov rdi, [rbp - 8]
	call print_int
	add rsp, 4
	lea r13, [.LC0]
	mov rdi, r13
	call print_string
	add rsp, 64
	lea r14, [.LC1]
	mov [rbp - 16], r14
	mov rdi, [rbp - 16]
	call print_string
	add rsp, 64
	lea r15, [.LC0]
	mov rdi, r15
	call print_string
	add rsp, 64
	xor rax, rax
	mov rsp, rbp
	pop rbp
	ret
