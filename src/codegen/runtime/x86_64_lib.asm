	global exit, string_length, print_string, print_char, print_newline
	global print_uint, print_int, read_char, read_word, parse_uint
	global parse_int, string_equals, string_copy
	global memcpy, malloc, free, clrscr, string_concat

	section .text

; rdi <- exit code
; exits program with provided `rdi` value
exit:
	; mov rax, 60
  mov rax, 0x2000001
	syscall

; rdi <- address of string
; rax -> length of string
string_length:
	xor rax, rax	; initialize to zero
.loop:
	cmp byte[rdi + rax], 0
	jz .end			; jump if we were on the null-byte

	inc rax
	jmp .loop
.end:
	ret

; rdi <- address of null-terminated string
; prints to stdout
print_string:
	call string_length
	mov rdx, rax
	mov rsi, rdi
	mov rdi, 1

	; mov rax, 1
  mov rax, 0x2000004
	syscall
	ret

; rdi <- char code
; prints to stdout
print_char:
	push rdi	; push it to the stack so we can get the address
	mov rsi, rsp	; start at this address, treating it as a string
	mov rdi, 1
	mov rdx, 1

	; mov rax, 1	; sys_write the char that is now a string
  mov rax, 0x2000004
  syscall
	add rsp, 8	; restore the address
	ret

; prints newline (0x0A) to stdout
print_newline:
	mov rdi, 1

	push 0x0A	; get rsi to point to an address with 0x0A content.
	mov rsi, rsp	; alternative to having a newline buffer in .data.

	mov rdx, 1
	; mov rax, 1
  mov rax, 0x2000004
	syscall
	add rsp, 8	; undo the 0x0A push
	ret

; rdi <- unsigned 64-bit integer
; prints in decimal format to stdout
print_uint:
	; converts integer into a null-term string, passed to print_string
	sub rsp, 32	; the max number of digits of uint64_t is ~20
	mov rsi, rsp
	add rsi, 31	; get to the highest byte, last char (little endian)
	mov byte[rsi], 0; plant the null terminator
; rdi <- value to convert
; rsi <- current position (starting at null-terminator)
.convert:
	mov rax, rdi	; dividend low part
	xor rdx, rdx	; dividend high part
	mov rcx, 10	; divisor
	div rcx		; [rax]/10, quotient := rax, remainder := rdx
	add dl, 0x30	; add '0' char to remainder byte for ascii value
	dec rsi		; decrease to the next valid space in the stack buffer
	mov [rsi], dl
	mov rdi, rax	; update value to convert with new value before looping
	test rdi, rdi
	jnz .convert

	mov rdi, rsi	; setup arg, pointer to start of string
	call print_string
	add rsp, 32	; restore the stack
	ret

; rdi <- signed 64-bit integer
; prints in decimal format to stdout
print_int:
	; converts integer into a null-term string, passed to print_string
	sub rsp, 32	; arbitrary large buffer size
	mov rsi, rsp
	add rsi, 31	; get to the highest byte, last char (little endian)
	mov byte[rsi], 0; plant the null terminator

	mov rax, rdi	; save the original value

	xor r8, r8	; set sign flag to zero
	cmp rdi, 0
	jge .convert

	neg rdi
	mov r8, 1	; set sign flag to one
	jmp .convert
; rdi <- value to convert
; rsi <- current position (starting at null-terminator)
; r8 <- sign flag, 0=positive, 1=negative
.convert:
	mov rax, rdi	; dividend low part
	xor rdx, rdx	; dividend high part
	mov rcx, 10	; divisor
	div rcx		; [rax]/10, quotient := rax, remainder := rdx
	add dl, 0x30	; add '0' char to remainder byte for ascii value
	dec rsi		; decrease to the next valid space in the stack buffer
	mov [rsi], dl
	mov rdi, rax	; update value to convert with new value before looping
	test rdi, rdi
	jnz .convert

	; add '-' if the number was negative
	test r8, r8
	jz .done
	dec rsi
	mov byte[rsi], 0x2D ; '-' char
.done:
	mov rdi, rsi
	call print_string
	add rsp, 32	; restore the stack
	ret

; al -> character read from stdin, else 0 if the end of input stream occurs
read_char:
	; mov rax, 0	; sys_read
  mov rax, 0x2000003
	mov rdi, 0	; stdin
	sub rsp, 8	; make room for 8 bytes on stack
	mov rsi, rsp	; ptr to new space on the stack
	mov rdx, 1	; only want to read 1 byte
	syscall
	test rax, rax
	jz .eof
	mov al, byte[rsp]
	jmp .done
.eof:
	xor rax, rax	; sets al to zero
.done:
	add rsp, 8	; restore context
	ret

; rdi <- address of buffer
; rsi <- size of buffer
; rax -> buffer address, else 0 if word doesn't fit the buffer
; reads next word from stdin (skipping whitespace) into the provided buffer.
; the returned buffer will be null-terminated.
read_word:
	xor rdx, rdx	; clear the index counter
.skip_ws:
	; skip any leading whitespace
	call .call_read_char
	test al, al
	jz .eof
	cmp al, 0x20	; space
	je .skip_ws
	cmp al, 0x09	; tab
	je .skip_ws
	cmp al, 0x0A	; newline
	je .skip_ws
.loop:
	cmp rdx, rsi-1	; account for the space needed for the null byte
	jae .err
	mov [rdi + rdx], al	; store the valid character that was last read
	inc rdx

	; break the loop on any trailing whitespace
	call .call_read_char
	test al, al
	jz .terminate
	cmp al, 0x20	; space
	je .terminate
	cmp al, 0x09	; tab
	je .terminate
	cmp al, 0x0A	; newline
	je .terminate
	jmp .loop	; else, we loop and store the value at the top of loop
.eof:
	test rdx, rdx
	jz .err		; if rdx is not zero, then we should fall-through
.terminate:
	; null-terminate
	mov byte[rdx + rdi], 0
	mov rax, rdi
	ret
.call_read_char:
	push rdi
	push rsi
	push rdx
	call read_char
	pop rdx
	pop rsi
	pop rdi
	ret
.err:
	xor rax, rax	; zero on overflow
	ret


; rdi <- address of null-terminated string
; rax -> parsed unsigned number
; rdx -> character count
parse_uint:
	xor rax, rax
	xor rdx, rdx
.loop:
	xor rcx, rcx	; clear prior to storage
	mov cl, byte[rdi + rdx]
	test cl, cl
	jz .done
	cmp cl, 0x30	; '0' char
	jb .done	; done if cl is below 0
	cmp cl, 0x39	; '9' char
	ja .done	; done if cl is above 9
	sub cl, 0x30	; get number from ascii (- '0')
	imul rax, 10	; rax := rax * 10
	add rax, rcx
	inc rdx
	jmp .loop
.done:
	ret

; rdi <- address of null-terminated string
; rax -> parsed signed number
; rdx -> character count (including sign if any)
; no space between sign and digits are allowed.
parse_int:
	xor r8, r8	; sign flag, 0 = positive, 1 = negative
	xor rsi, rsi	; start of unsigned portion
	mov cl, byte[rdi]
	cmp cl, 0x2D	; '-' char
	je .is_negative
	cmp cl, 0x2B	; '+' char
	je .is_positive
	jmp .parse
.is_negative:
	mov r8, 1
.is_positive:
	mov rsi, 1
	inc rdi		; move past the sign
.parse:
	call parse_uint	; sets rax (num) and rdx (size)
	add rdx, rsi	; add possible sign character to the length
	test r8, r8	; check sign
	jz .done
	neg rax
.done:
	ret

; rdi <- address of string
; rsi <- address of string
; rax -> 1 if they are equal, else 0
string_equals:
	call string_length
	mov rcx, rax	; save first len in rcx
	mov rdx, rdi	; temp save of first string
	mov rdi, rsi	; first = second
	mov rsi, rdx	; second = first (temp)
	call string_length
	cmp rcx, rax	; compare first len with second len (in rax)
	jne .not_equal
.loop:
	mov al, byte[rdi]
	mov cl, byte[rsi]
	cmp al, cl
	jne .not_equal

	test al, al	; check if we reached a null byte
	jz .equal

	inc rdi
	inc rsi
	jmp .loop
.equal:
	mov rax, 1
	ret
.not_equal:
	xor rax, rax
	ret

; rdi <- address of string
; rsi <- address of buffer
; rdx <- length of buffer
; rax -> buffer address, else 0 if the string doesn't fit the buffer
; copies the string from rdi into the buffer at rsi
string_copy:
	xor rcx, rcx
.loop:
	cmp rcx, rdx
	jae .err
	mov al, [rdi + rcx]
	mov [rsi + rcx], al
	inc rcx
	test al, al
	jnz .loop
	mov rax, rsi
	ret
.err:
	xor rax, rax
	ret

; rdi <- dst
; rsi <- src
; rcx <- size
; copies 'size' bytes from src to dst. requires dst to be allocated prior
memcpy:
	cld           ; clear direction flag
	rep movsb     ; repeat movsb from [rsi] to [rdi], rcx times, incrementing both each time
	ret

%define SIGNATURE 0xDEADC0DEDEADC0DE
; rdi <- size in bytes
; rax -> allocated memory address (16-byte aligned), or 0 on error
malloc:
  test rdi, rdi
  jz .invalid_size
  add rdi, 16        ; total_size = requested_size + 16
  jc .error
  mov rsi, rdi       ; length = total_size
  xor rdi, rdi       ; addr = NULL (let kernel choose)
  mov rdx, 3         ; prot = PROT_READ | PROT_WRITE
  mov r10, 0x1002    ; flags = MAP_ANON | MAP_PRIVATE
  mov r8, -1         ; fd = -1
  xor r9, r9         ; offset = 0
  mov rax, 0x20000C5 ; mmap syscall
  syscall
  cmp rax, -1
  je .error
  mov r8, SIGNATURE
  mov QWORD[rax], r8      ; store signature for identification in 'free'
  mov QWORD[rax + 8], rsi ; store total_size at the start of the block
  add rax, 16             ; return address after header
  ret
  .invalid_size:
  xor rax, rax
  ret
  .error:
  xor rax, rax
  ret

; rdi <- memory address to free (must be from malloc or NULL)
free:
  test rdi, rdi
  jz .end             ; free(NULL) is safe no-op, we will just return back

  mov rax, [rdi - 16] ; check for the signature added during malloc
  mov rsi, SIGNATURE
  cmp rax, rsi
  jne .err            ; signature mismatch: not our block from malloc

  mov rsi, [rdi - 8]  ; get our total size from the header from malloc
  lea rdi, [rdi - 16] ; get base address
  mov rax, 0x2000049  ; munmap syscall
  syscall
  jmp .end
  .err:
  mov rdi, free_err
  call print_string
  .end:
  ret

clrscr:
  ; I tried to make this have no side effects, so that the user can somewhat
  ; safely run `asm { call clrscr };`, as I don't want to have that in the lang
  push rdi
  push rsi
  push rdx
  push rax
  mov rax, 0x2000004
  mov rdi, 1
  mov rsi, clr_scr ; load address to rsi
  mov rdx, 7
  syscall
  pop rax
  pop rdx
  pop rsi
  pop rdi
  ret

; rdi <- addr of first null-terminated str
; rsi <- addr of second null-terminated str
; rax -> addr of new null-terminated str (heap-allocated; caller must free)
string_concat:
  push rbx
  push rcx
  push rdx
  push r12
  push r13
  push r14
  push r15

  mov r12, rdi        ; save first string pointer
  mov r13, rsi        ; save second string pointer

  mov rdi, r12        ; arg1: first string
  call string_length
  mov r14, rax        ; r10 = len1

  mov rdi, r13        ; arg1: second string
  call string_length
  mov r15, rax       ; r11 = len2

  mov rax, r14
  add rax, r15
  add rax, 1         ; total length = len1 + len2 + 1 (for null)
  mov rdi, rax       ; arg1: malloc size
  call malloc
  test rax, rax
  jz .err            ; malloc failed
  mov rbx, rax       ; rbx = dest buffer

  ; copy first string
  mov rdi, rbx       ; dest
  mov rsi, r12       ; src
  mov rcx, r14       ; size of string 1
  call memcpy

  ; copy second string
  lea rdi, [rbx + r14] ; dest = buffer + len1
  mov rsi, r13         ; src = second string
  mov rcx, r15         ; size of string 2
  call memcpy

  ; null-terminate
  lea rax, [r14 + r15]
  mov byte [rbx + rax], 0
  mov rax, rbx         ; return pointer
  jmp .epilogue
  .err:
  xor rax, rax
  .epilogue:
  pop r15
  pop r14
  pop r13
  pop r12
  pop rdx
  pop rcx
  pop rbx
  ret

section .data
  clr_scr: db 0x1B, '[', '2', 'J', 0x1B, '[', 'H'  ; "\x1B[2J\x1B[H"
  free_err: db "[ASM Error] Invalid free of memory not allocated by 'malloc' / not 16-bit aligned", 10, 0
