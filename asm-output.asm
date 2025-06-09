extern exit, string_length, print_string, print_char
extern print_newline, print_uint, print_int
extern read_char, read_word, parse_uint
extern parse_int, string_equals, string_copy

global _start
section .text
_start:
	push rbp
	mov rbp, rsp
	push r12
	push r13
	push r14
	push r15
	push rbx
	mov r10, 1
	mov r11, 3
	push rax
	cmp r10, r11
	setne al
	movzx r12, al
	pop rax
	test r12, r12
	jz _L0
	mov r13, 2
	push rax
	cmp r10, r13
	setne al
	movzx r14, al
	pop rax
	test r14, r14
	jz _L1
	mov r15, 1
	push rax
	cmp r10, r15
	setne al
	movzx rbx, al
	pop rax
	test rbx, rbx
	jz _L2
	jmp _L3
_L0:
	mov r10, LC0
	mov rdi, r10
	call print_string
	jmp _L4
_L1:
	mov r11, LC1
	mov rdi, r11
	call print_string
	jmp _L4
_L2:
	mov r12, LC2
	mov rdi, r12
	call print_string
	jmp _L4
_L3:
	mov r13, LC3
	mov rdi, r13
	call print_string
_L4:
	pop rbx
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp ; restore stack
	pop rbp
	xor rdi, rdi
	call exit

section .data
LC0:
	db "it was three", 10, "", 0
LC1:
	db "it was two", 10, "", 0
LC2:
	db "it was one", 10, "", 0
LC3:
	db "i don't know what it was", 10, "", 0
