extern exit, string_length, print_string, print_char
extern print_newline, print_uint, print_int
extern read_char, read_word, parse_uint
extern parse_int, string_equals, string_copy
extern malloc, free

default rel
global _start
section .text
_start:
	push rbp
	mov rbp, rsp
	sub rsp, 48
	push r12
	push r13
	push r14
	push r15
	push rbx
	lea r10, _foo
	mov QWORD[global_vars + 8], r10
	mov r11, 1
	mov rdi, r11
	mov r12, 2
	mov rsi, r12
	mov r13, 3
	mov rdx, r13
	call _foo
	mov r14, 4
	mov rdi, r14
	mov r15, 5
	mov rsi, r15
	mov rbx, 6
	mov rdx, rbx
	call QWORD[global_vars + 8]
	mov r10, 3
	mov QWORD[global_vars + 16], r10
	lea r11, [global_vars + 16]
	mov QWORD[global_vars + 24], r11
	mov r12, LC0
	mov rdi, r12
	call print_string
	mov r13, QWORD[global_vars + 24]
	mov r13, [r13]
	mov rdi, r13
	call print_int
	mov r14, LC1
	mov rdi, r14
	call print_string
	mov rdi, QWORD[global_vars + 16]
	call print_int
	mov r15, LC2
	mov rdi, r15
	call print_string
	lea rbx, [global_vars + 16]
	mov QWORD[global_vars + 32], rbx
	mov r10, 15
	mov QWORD[rbp - 8], r11 ; spilling register
	mov r11, QWORD[global_vars + 32]
	mov [r11], r10
	mov r12, LC3
	mov rdi, r12
	call print_string
	mov r13, LC4
	mov rdi, r13
	call print_string
	mov r14, QWORD[global_vars + 32]
	mov r14, [r14]
	mov rdi, r14
	call print_int
	mov r15, LC1
	mov rdi, r15
	call print_string
	mov rbx, QWORD[global_vars + 24]
	mov rbx, [rbx]
	mov rdi, rbx
	call print_int
	mov r10, LC1
	mov rdi, r10
	call print_string
	mov rdi, QWORD[global_vars + 16]
	call print_int
	mov r11, LC2
	mov rdi, r11
	call print_string
	mov r12, 0
	mov QWORD[global_vars + 24], r12
	mov r13, LC5
	mov rdi, r13
	call print_string
	mov rdi, QWORD[global_vars + 24]
	call print_int
	mov r14, LC2
	mov rdi, r14
	call print_string
	lea r15, [global_vars + 16]
	mov QWORD[global_vars + 24], r15
	mov rbx, LC6
	mov rdi, rbx
	call print_string
	mov rdi, QWORD[global_vars + 24]
	call print_int
	mov r10, LC2
	mov rdi, r10
	call print_string
	mov r11, QWORD[global_vars + 24]
	mov r11, [r11]
	mov rdi, r11
	call print_int
	mov r12, LC2
	mov rdi, r12
	call print_string
	mov r13, 0
	mov r14, QWORD[global_vars + 24]
	add r14, r13
	mov r15, r14
	mov r15, [r15]
	mov rdi, r15
	call print_int
	mov rbx, LC2
	mov rdi, rbx
	call print_string
	mov r10, 0
	mov r11, r10
	imul r11, 8
	mov r12, QWORD[global_vars + 24]
	add r12, r11
	mov r13, r12
	mov r13, [r13]
	mov rdi, r13
	call print_int
	mov r14, LC2
	mov rdi, r14
	call print_string
	mov QWORD[global_vars + 40], 0
	mov r15, 3
	mov QWORD[rbp - 16], r10 ; spilling register
	mov r10, r15
	imul r10, 8
	mov rdi, r10
	call malloc
	mov rbx, rax
	mov QWORD[global_vars + 40], rbx
	mov r11, 30
	mov r12, 0
	mov r13, r12
	imul r13, 8
	mov r14, QWORD[global_vars + 40]
	add r14, r13
	mov QWORD[rbp - 24], r15 ; spilling register
	mov r15, r14
	mov [r15], r11
	mov rbx, 60
	mov r10, 1
	mov r11, r10
	imul r11, 8
	mov r12, QWORD[global_vars + 40]
	add r12, r11
	mov QWORD[rbp - 32], r13 ; spilling register
	mov r13, r12
	mov [r13], rbx
	mov r14, 90
	mov r15, 2
	mov rbx, r15
	imul rbx, 8
	mov r10, QWORD[global_vars + 40]
	add r10, rbx
	mov QWORD[rbp - 40], r11 ; spilling register
	mov r11, r10
	mov [r11], r14
	mov rdi, QWORD[global_vars + 40]
	mov r12, 3
	mov rsi, r12
	call _print_arr
	mov rdi, QWORD[global_vars + 40]
	call free
	mov r13, 0
	mov QWORD[global_vars + 40], r13
	mov r14, 32
	mov rdi, 8
	call malloc
	mov r15, rax
	mov QWORD[rax], r14
	mov QWORD[global_vars + 40], r15
	mov rdi, QWORD[global_vars + 40]
	mov rbx, 1
	mov rsi, rbx
	call _print_arr
	mov rdi, QWORD[global_vars + 40]
	call free
	pop rbx
	pop r15
	pop r14
	pop r13
	pop r12
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
	mov DWORD[rbp - 4], edi
	mov DWORD[rbp - 8], esi
	mov DWORD[rbp - 12], edx
	mov r10, LC7
	mov rdi, r10
	call print_string
	mov edi, DWORD[rbp - 4]
	call print_int
	mov r11, LC2
	mov rdi, r11
	call print_string
	mov edi, DWORD[rbp - 8]
	call print_int
	mov r12, LC2
	mov rdi, r12
	call print_string
	mov edi, DWORD[rbp - 12]
	call print_int
	mov r13, LC2
	mov rdi, r13
	call print_string
	pop r13
	pop r12
	xor rax, rax ; void return
	mov rsp, rbp ; restore stack
	pop rbp
	ret
_print_arr:
	push rbp
	mov rbp, rsp
	sub rsp, 32
	push r12
	push r13
	push r14
	push r15
	push rbx
	mov QWORD[rbp - 8], rdi
	mov QWORD[rbp - 16], rsi
	mov QWORD[rbp - 24], 0
_L0:
	push rax
	mov r11, QWORD[rbp - 16]
	cmp QWORD[rbp - 24], r11
	setl al
	movzx r10, al
	pop rax
	test r10b, r10b
	jz _L3
_L2:
	mov r12, QWORD[rbp - 24]
	imul r12, 8
	mov r13, QWORD[rbp - 8]
	add r13, r12
	mov r14, r13
	mov r14, [r14]
	mov rdi, r14
	call print_int
	mov r15, LC2
	mov rdi, r15
	call print_string
_L1:
	mov rbx, 1
	mov r10, QWORD[rbp - 24]
	add r10, rbx
	mov QWORD[rbp - 24], r10
	jmp _L0
_L3:
	pop rbx
	pop r15
	pop r14
	pop r13
	pop r12
	xor rax, rax ; void return
	mov rsp, rbp ; restore stack
	pop rbp
	ret

section .data
LC0:
	db "[*px, x] = ", 0
LC1:
	db ", ", 0
LC2:
	db 10, 0
LC3:
	db "*py = 15;", 10, "", 0
LC4:
	db "[*py, *px, x] = ", 0
LC5:
	db "Null(addr): ", 0
LC6:
	db "Address: ", 0
LC7:
	db "Running foo_1", 10, "", 0
section .bss
	global_vars resb 48
