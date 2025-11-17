section .text
extern snek_error, snek_print
global our_code_starts_here
our_code_starts_here:
push rbp
mov rbp, rsp
sub rsp, 40
mov rax, 0
mov [rbp - 8], rax
mov rax, 0
mov [rbp - 16], rax
mov rax, 2
mov [rbp - 24], rax
mov rax, 0
mov [rbp - 32], rax
loop_start_0:
mov rax, [rbp - 8]
mov [rbp - 40], rax
mov rax, 10
mov rcx, [rbp - 40]
cmp rax, 3
jz eq_right_bool_1
cmp rax, 1
jz eq_right_bool_1
cmp rcx, 3
jz eq_err_1
cmp rcx, 1
jz eq_err_1
jmp eq_types_ok_1
eq_right_bool_1:
cmp rcx, 3
jz eq_types_ok_1
cmp rcx, 1
jz eq_types_ok_1
jmp eq_err_1
eq_err_1:
mov rdi, -11
call snek_error
eq_types_ok_1:
cmp rax, rcx
mov rax, 0
sete al
shl rax, 1
add rax, 1
cmp rax, 3
jz if_then_2
cmp rax, 1
jz if_else_2
jmp if_err_2
if_then_2:
mov rax, [rbp - 16]
jmp loop_end_0
jmp if_end_2
if_else_2:
mov rax, [rbp - 16]
mov [rbp - 40], rax
mov rax, [rbp - 24]
mov rcx, [rbp - 40]
cmp rax, 3
jz err_ar_3
cmp rax, 1
jz err_ar_3
cmp rcx, 3
jz err_ar_3
cmp rcx, 1
jz err_ar_3
jmp ok_ar_3
err_ar_3:
mov rdi, -1
call snek_error
ok_ar_3:
add rax, rcx
jo of_ar_4
jmp cont_ar_4
of_ar_4:
mov rdi, -9
call snek_error
cont_ar_4:
mov rax, [rbp - 16]
mov [rbp - 40], rax
mov rax, [rbp - 32]
mov rcx, [rbp - 40]
cmp rax, 3
jz err_ar_5
cmp rax, 1
jz err_ar_5
cmp rcx, 3
jz err_ar_5
cmp rcx, 1
jz err_ar_5
jmp ok_ar_5
err_ar_5:
mov rdi, -1
call snek_error
ok_ar_5:
add rax, rcx
jo of_ar_6
jmp cont_ar_6
of_ar_6:
mov rdi, -9
call snek_error
cont_ar_6:
mov rax, [rbp - 24]
mov [rbp - 32], rax
mov rax, [rbp - 16]
mov [rbp - 24], rax
mov rax, [rbp - 8]
mov [rbp - 40], rax
mov rax, 2
mov rcx, [rbp - 40]
cmp rax, 3
jz err_ar_7
cmp rax, 1
jz err_ar_7
cmp rcx, 3
jz err_ar_7
cmp rcx, 1
jz err_ar_7
jmp ok_ar_7
err_ar_7:
mov rdi, -1
call snek_error
ok_ar_7:
add rax, rcx
jo of_ar_8
jmp cont_ar_8
of_ar_8:
mov rdi, -9
call snek_error
cont_ar_8:
mov [rbp - 8], rax
jmp if_end_2
if_err_2:
mov rdi, -7
call snek_error
if_end_2:
jmp loop_start_0
loop_end_0:
add rsp, 40
pop rbp
ret
