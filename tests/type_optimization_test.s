section .text
extern snek_error, snek_print
global our_code_starts_here
fun_test_optimization:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
cmp rax, 3
jz isbool_set_true_0
cmp rax, 1
jz isbool_set_true_0
mov rax, 1
jmp isbool_done_0
isbool_set_true_0:
mov rax, 3
isbool_done_0:
cmp rax, 3
jz if_then_1
cmp rax, 1
jz if_else_1
jmp if_err_1
if_then_1:
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 3
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
jmp if_end_1
if_else_1:
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 20
mov rcx, [rbp - 8]
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
jmp if_end_1
if_err_1:
mov rdi, -7
call snek_error
if_end_1:
add rsp, 8
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, 10
push rax
call fun_test_optimization
add rsp, 8
pop rbp
ret
