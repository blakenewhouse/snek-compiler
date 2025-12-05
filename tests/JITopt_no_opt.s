section .text
extern snek_error, snek_print
global our_code_starts_here
fun_abs:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
mov rcx, rax
cmp rcx, 3
jz cast_num_err_0
cmp rcx, 1
jz cast_num_err_0
jmp cast_num_ok_0
cast_num_err_0:
mov rdi, -19
call snek_error
cast_num_ok_0:
mov [rbp - 8], rax
mov rax, 0
mov rcx, [rbp - 8]
cmp rax, 3
jz err_cmp_1
cmp rax, 1
jz err_cmp_1
cmp rcx, 3
jz err_cmp_1
cmp rcx, 1
jz err_cmp_1
jmp ok_cmp_1
err_cmp_1:
mov rdi, -3
call snek_error
ok_cmp_1:
cmp rcx, rax
mov rax, 0
setl al
shl rax, 1
add rax, 1
cmp rax, 3
jz if_then_2
cmp rax, 1
jz if_else_2
jmp if_err_2
if_then_2:
mov rax, 0
mov [rbp - 8], rax
mov rax, [rbp + 16]
mov rcx, rax
cmp rcx, 3
jz cast_num_err_3
cmp rcx, 1
jz cast_num_err_3
jmp cast_num_ok_3
cast_num_err_3:
mov rdi, -19
call snek_error
cast_num_ok_3:
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
sub rcx, rax
jo of_ar_5
mov rax, rcx
jmp cont_ar_5
of_ar_5:
mov rdi, -9
call snek_error
cont_ar_5:
jmp if_end_2
if_else_2:
mov rax, [rbp + 16]
jmp if_end_2
if_err_2:
mov rdi, -7
call snek_error
if_end_2:
add rsp, 8
pop rbp
ret
our_code_starts_here:
  mov rdi, 20
push rbp
mov rbp, rsp
sub rsp, 8
; Block:
mov rax, 10
mov [rbp - 8], rax
mov rax, rdi
mov rcx, [rbp - 8]
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
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, rdi
push rax
call fun_abs
add rsp, 8
add rsp, 8
pop rbp
ret
