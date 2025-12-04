section .text
extern snek_error, snek_print
global our_code_starts_here
fun_m:
push rbp
mov rbp, rsp
sub rsp, 16
; Let Binding: factor
mov rax, [rbp + 24]
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
mov rax, [rbp + 24]
mov rcx, rax
cmp rcx, 3
jz cast_bool_ok_2
cmp rcx, 1
jz cast_bool_ok_2
cast_bool_err_2:
mov rdi, -19
call snek_error
cast_bool_ok_2:
cmp rax, 3
jz if_then_3
cmp rax, 1
jz if_else_3
jmp if_err_3
if_then_3:
mov rax, 2
jmp if_end_3
if_else_3:
mov rax, -2
jmp if_end_3
if_err_3:
mov rdi, -7
call snek_error
if_end_3:
jmp if_end_1
if_else_1:
mov rax, [rbp + 24]
mov rcx, rax
cmp rcx, 3
jz cast_num_err_4
cmp rcx, 1
jz cast_num_err_4
jmp cast_num_ok_4
cast_num_err_4:
mov rdi, -19
call snek_error
cast_num_ok_4:
jmp if_end_1
if_err_1:
mov rdi, -7
call snek_error
if_end_1:
mov [rbp - 8], rax
mov rax, [rbp + 16]
mov [rbp - 16], rax
mov rax, [rbp - 8]
mov rcx, [rbp - 16]
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
sar rax, 1
sar rcx, 1
imul rax, rcx
shl rax, 1
jo of_ar_6
jmp cont_ar_6
of_ar_6:
mov rdi, -9
call snek_error
cont_ar_6:
add rsp, 16
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, rdi
push rax
mov rax, 8
push rax
call fun_m
add rsp, 16
pop rbp
ret
