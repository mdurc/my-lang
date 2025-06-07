extern exit, string_length, print_string, print_char
extern print_newline, print_uint, print_int
extern read_char, read_word, parse_uint
extern parse_int, string_equals, string_copy

section .data
LC0:
	db "FizzBuzz", 10, "", 0
LC1:
	db "Fizz", 10, "", 0
LC2:
	db "Buzz", 10, "", 0
LC3:
	db 10, 0

global _start
section .text
_start:
	call main
	mov rdi, rax
	call exit
main:
	push rbp
	mov rbp, rsp
	sub rsp, 16
	push r12
	push r13
	push r14
	push r15
	push rbx
	mov r10, 1000
	mov DWORD[rbp - 4], r10d
	mov r11, 1
	mov DWORD[rbp - 8], r11d
_L0:
	push rax
	mov r13d, DWORD[rbp - 8]
	cmp r13d, DWORD[rbp - 4]
	setle al
	movzx r12d, al
	pop rax
	test r12b, r12b
	jz _L1
	mov r14, 15
	; --- MOD start ---
	push rax
	push rdx
	mov eax, DWORD[rbp - 8]
	cqo
	idiv r14d
	mov r15d, edx
	pop rdx
	pop rax
	; --- MOD end ---
	mov rbx, 0
	push rax
	cmp r15, rbx
	sete al
	movzx r10, al
	pop rax
	test r10b, r10b
	jz _L2
	mov r11, LC0
	mov rdi, r11
	call print_string
	jmp _L3
_L2:
	mov r12, 3
	; --- MOD start ---
	push rax
	push rdx
	mov eax, DWORD[rbp - 8]
	cqo
	idiv r12d
	mov r13d, edx
	pop rdx
	pop rax
	; --- MOD end ---
	mov r14, 0
	push rax
	cmp r13, r14
	sete al
	movzx r15, al
	pop rax
	test r15b, r15b
	jz _L4
	mov rbx, LC1
	mov rdi, rbx
	call print_string
	jmp _L5
_L4:
	mov r10, 5
	; --- MOD start ---
	push rax
	push rdx
	mov eax, DWORD[rbp - 8]
	cqo
	idiv r10d
	mov r11d, edx
	pop rdx
	pop rax
	; --- MOD end ---
	mov r12, 0
	push rax
	cmp r11, r12
	sete al
	movzx r13, al
	pop rax
	test r13b, r13b
	jz _L6
	mov r14, LC2
	mov rdi, r14
	call print_string
	jmp _L7
_L6:
	mov edi, DWORD[rbp - 8]
	call print_int
	mov r15, LC3
	mov rdi, r15
	call print_string
_L7:
_L5:
_L3:
	mov rbx, 1
	mov r10d, DWORD[rbp - 8]
	add r10d, ebx
	mov DWORD[rbp - 8], r10d
	jmp _L0
_L1:
	pop rbx
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp ; restore stack
	pop rbp
	xor rax, rax ; void return
	ret
