section .text
extern snek_error, snek_print
global our_code_starts_here
fun_sum3:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, [rbp + 24]
mov [rbp - 16], rax
mov rax, [rbp + 32]
mov rcx, [rbp - 16]
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
mov rcx, [rbp - 8]
cmp rax, 3
jz err_ar_2
cmp rax, 1
jz err_ar_2
cmp rcx, 3
jz err_ar_2
cmp rcx, 1
jz err_ar_2
jmp ok_ar_2
err_ar_2:
mov rdi, -1
call snek_error
ok_ar_2:
add rax, rcx
jo of_ar_3
jmp cont_ar_3
of_ar_3:
mov rdi, -9
call snek_error
cont_ar_3:
add rsp, 8
pop rbp
ret
fun_main:
push rbp
mov rbp, rsp
mov rax, [rbp + 32]
push rax
mov rax, [rbp + 24]
push rax
mov rax, [rbp + 16]
push rax
call fun_sum3
add rsp, 24
push rdi
mov rdi, rax
call snek_print
pop rdi
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, 6
push rax
mov rax, 4
push rax
mov rax, 2
push rax
call fun_main
add rsp, 24
pop rbp
ret
