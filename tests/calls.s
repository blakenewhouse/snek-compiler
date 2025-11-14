section .text
extern snek_error, snek_print
global our_code_starts_here
fun_inc:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 2
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
fun_sum:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, [rbp + 24]
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
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, 2
sub rsp, 8
mov [rsp + 0], rax
call fun_inc
add rsp, 8
sub rsp, 8
mov [rsp + 0], rax
mov rax, 80
sub rsp, 8
mov [rsp + 0], rax
call fun_sum
add rsp, 16
pop rbp
ret
