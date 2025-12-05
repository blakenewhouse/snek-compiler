section .text
extern snek_error, snek_print
global our_code_starts_here
our_code_starts_here:
push rbp
mov rbp, rsp
; Let Binding: f
mov rax, 24
mov [rbp - 8], rax
mov rax, [rbp - 8]
mov [rbp - 16], rax
mov rax, 4
mov rcx, rax
mov rax, [rbp - 16]
; Optimized: skip tag checks for Plus (both Num)
add rax, rcx
jo of_binop_0
jmp cont_binop_0
of_binop_0:
mov rdi, -9
call snek_error
cont_binop_0:
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rsp, rbp
pop rbp
ret
