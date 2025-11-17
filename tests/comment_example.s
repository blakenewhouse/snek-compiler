section .text
extern snek_error, snek_print
global our_code_starts_here
our_code_starts_here:
push rbp
mov rbp, rsp
sub rsp, 24
; Let bindings start
; Binding: x
mov rax, 10
mov [rbp - 8], rax
; Binding: y
mov rax, 20
mov [rbp - 16], rax
; Let body expression
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, [rbp - 16]
mov rcx, [rbp - 24]
cmp rax, 3
jz err_ar_0
cmp rax, 1
jz err_ar_0
cmp rcx, 3
jz err_ar_0
cmp rcx, 1
jz err_ar_0
jmp ok_ar_0
err_ar_0:
mov rdi, -1
call snek_error
ok_ar_0:
add rax, rcx
jo of_ar_1
jmp cont_ar_1
of_ar_1:
mov rdi, -9
call snek_error
cont_ar_1:
add rsp, 24
pop rbp
ret
