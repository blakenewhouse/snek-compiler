section .text
extern snek_error, snek_print
global our_code_starts_here
fun_multiply:
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
sar rax, 1
sar rcx, 1
imul rax, rcx
shl rax, 1
jo of_ar_1
jmp cont_ar_1
of_ar_1:
mov rdi, -9
call snek_error
cont_ar_1:
add rsp, 8
pop rbp
ret
fun_add_nums:
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
fun_return_bool:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, 10
mov [rbp - 8], rax
mov rax, 6
mov rcx, [rbp - 8]
cmp rax, 3
jz err_cmp_4
cmp rax, 1
jz err_cmp_4
cmp rcx, 3
jz err_cmp_4
cmp rcx, 1
jz err_cmp_4
jmp ok_cmp_4
err_cmp_4:
mov rdi, -3
call snek_error
ok_cmp_4:
cmp rcx, rax
mov rax, 0
setg al
shl rax, 1
add rax, 1
add rsp, 8
pop rbp
ret
fun_return_num:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 2
mov rcx, [rbp - 8]
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
add rsp, 8
pop rbp
ret
fun_mixed:
push rbp
mov rbp, rsp
mov rax, [rbp + 24]
cmp rax, 3
jz if_then_7
cmp rax, 1
jz if_else_7
jmp if_err_7
if_then_7:
mov rax, [rbp + 16]
jmp if_end_7
if_else_7:
mov rax, 0
jmp if_end_7
if_err_7:
mov rdi, -7
call snek_error
if_end_7:
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, 8
push rax
mov rax, 6
push rax
call fun_multiply
add rsp, 16
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, 40
push rax
mov rax, 20
push rax
call fun_add_nums
add rsp, 16
push rdi
mov rdi, rax
call snek_print
pop rdi
call fun_return_bool
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, 84
push rax
call fun_return_num
add rsp, 8
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, 3
push rax
mov rax, 200
push rax
call fun_mixed
add rsp, 16
pop rbp
ret
