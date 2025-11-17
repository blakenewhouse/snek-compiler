section .text
extern snek_error, snek_print
global our_code_starts_here
fun_add:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, [rbp + 24]
mov rcx, [rbp - 8]
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
add rsp, 8
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, 40
push rax
mov rax, 20
push rax
call fun_add
add rsp, 16
pop rbp
ret
