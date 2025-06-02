extern exit, string_length, print_string, print_char
extern print_newline, print_uint, print_int
extern read_char, read_word, parse_uint
extern parse_int, string_equals, string_copy

section .data
.LC0:
	db 10, 0
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
	sub rsp, 64 ; function preamble

	mov r10, 0 ; default initialization
	mov [rbp - 8], r10 ; store the value of 'a' within -8
	mov r11, 2 ; tmp for immediate 2

	mov rbx, r11
	add rbx, [rbp - 8] ; add '2 + a'

	mov [rbp - 8], rbx ; store result in 'a'

	mov r12, 3
	mov [rbp - 8], r12 ; 'a = 3'

	mov rdi, [rbp - 8]
	call print_int ; setup 'a' as print arg and print

	mov r13, .LC0 ; assignment to string
	mov rdi, r13 ; pushed as argument
	call print_string ; syscall

	mov r14, .LC1
	mov [rbp - 16], r14 ; store string in 'b'
	mov rdi, [rbp - 16] ; push as arg
	call print_string ; syscall

	mov r15, .LC0
	mov rdi, r15
	call print_string

	xor rax, rax ; return zero
	mov rsp, rbp
	pop rbp
	ret
