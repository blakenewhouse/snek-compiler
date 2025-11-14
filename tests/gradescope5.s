section .text
extern snek_error, snek_print
global our_code_starts_here
fun_sum6:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, [rbp + 24]
mov [rbp - 16], rax
mov rax, [rbp + 32]
mov [rbp - 24], rax
mov rax, [rbp + 40]
mov [rbp - 32], rax
mov rax, [rbp + 48]
mov [rbp - 40], rax
mov rax, [rbp + 56]
mov rcx, [rbp - 40]
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
mov rcx, [rbp - 32]
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
mov rcx, [rbp - 24]
cmp rax, 3
jz err_ar_4
cmp rax, 1
jz err_ar_4
cmp rcx, 3
jz err_ar_4
cmp rcx, 1
jz err_ar_4
jmp ok_ar_4
err_ar_4:
mov rdi, -1
call snek_error
ok_ar_4:
add rax, rcx
jo of_ar_5
jmp cont_ar_5
of_ar_5:
mov rdi, -9
call snek_error
cont_ar_5:
mov rcx, [rbp - 16]
cmp rax, 3
jz err_ar_6
cmp rax, 1
jz err_ar_6
cmp rcx, 3
jz err_ar_6
cmp rcx, 1
jz err_ar_6
jmp ok_ar_6
err_ar_6:
mov rdi, -1
call snek_error
ok_ar_6:
add rax, rcx
jo of_ar_7
jmp cont_ar_7
of_ar_7:
mov rdi, -9
call snek_error
cont_ar_7:
mov rcx, [rbp - 8]
cmp rax, 3
jz err_ar_8
cmp rax, 1
jz err_ar_8
cmp rcx, 3
jz err_ar_8
cmp rcx, 1
jz err_ar_8
jmp ok_ar_8
err_ar_8:
mov rdi, -1
call snek_error
ok_ar_8:
add rax, rcx
jo of_ar_9
jmp cont_ar_9
of_ar_9:
mov rdi, -9
call snek_error
cont_ar_9:
add rsp, 8
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, 12
push rax
mov rax, 10
push rax
mov rax, 8
push rax
mov rax, 6
push rax
mov rax, 4
push rax
mov rax, 2
push rax
call fun_sum6
add rsp, 48
push rdi
mov rdi, rax
call snek_print
pop rdi
pop rbp
ret
