section .text
extern snek_error, snek_print
global our_code_starts_here
fun_isodd:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 0
mov rcx, [rbp - 8]
cmp rax, 3
jz err_cmp_0
cmp rax, 1
jz err_cmp_0
cmp rcx, 3
jz err_cmp_0
cmp rcx, 1
jz err_cmp_0
jmp ok_cmp_0
err_cmp_0:
mov rdi, -3
call snek_error
ok_cmp_0:
cmp rcx, rax
mov rax, 0
setl al
shl rax, 1
add rax, 1
cmp rax, 3
jz if_then_1
cmp rax, 1
jz if_else_1
jmp if_err_1
if_then_1:
mov rax, 0
mov [rbp - 8], rax
mov rax, [rbp + 16]
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
sub rcx, rax
jo of_ar_3
mov rax, rcx
jmp cont_ar_3
of_ar_3:
mov rdi, -9
call snek_error
cont_ar_3:
push rdi
mov rdi, rax
call snek_print
pop rdi
jmp if_end_1
if_else_1:
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 0
mov rcx, [rbp - 8]
cmp rax, 3
jz eq_right_bool_4
cmp rax, 1
jz eq_right_bool_4
cmp rcx, 3
jz eq_err_4
cmp rcx, 1
jz eq_err_4
jmp eq_types_ok_4
eq_right_bool_4:
cmp rcx, 3
jz eq_types_ok_4
cmp rcx, 1
jz eq_types_ok_4
jmp eq_err_4
eq_err_4:
mov rdi, -11
call snek_error
eq_types_ok_4:
cmp rax, rcx
mov rax, 0
sete al
shl rax, 1
add rax, 1
cmp rax, 3
jz if_then_5
cmp rax, 1
jz if_else_5
jmp if_err_5
if_then_5:
mov rax, 1
jmp if_end_5
if_else_5:
mov rax, [rbp + 16]
cmp rax, 3
jz err_unop_6
cmp rax, 1
jz err_unop_6
jmp ok_unop_6
err_unop_6:
mov rdi, -1
call snek_error
ok_unop_6:
sub rax, 2
jo of_unop_7
jmp cont_unop_7
of_unop_7:
mov rdi, -9
call snek_error
cont_unop_7:
push rax
call fun_iseven
add rsp, 8
jmp if_end_5
if_err_5:
mov rdi, -7
call snek_error
if_end_5:
jmp if_end_1
if_err_1:
mov rdi, -7
call snek_error
if_end_1:
add rsp, 8
pop rbp
ret
fun_iseven:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 0
mov rcx, [rbp - 8]
cmp rax, 3
jz eq_right_bool_8
cmp rax, 1
jz eq_right_bool_8
cmp rcx, 3
jz eq_err_8
cmp rcx, 1
jz eq_err_8
jmp eq_types_ok_8
eq_right_bool_8:
cmp rcx, 3
jz eq_types_ok_8
cmp rcx, 1
jz eq_types_ok_8
jmp eq_err_8
eq_err_8:
mov rdi, -11
call snek_error
eq_types_ok_8:
cmp rax, rcx
mov rax, 0
sete al
shl rax, 1
add rax, 1
cmp rax, 3
jz if_then_9
cmp rax, 1
jz if_else_9
jmp if_err_9
if_then_9:
mov rax, 3
jmp if_end_9
if_else_9:
mov rax, [rbp + 16]
cmp rax, 3
jz err_unop_10
cmp rax, 1
jz err_unop_10
jmp ok_unop_10
err_unop_10:
mov rdi, -1
call snek_error
ok_unop_10:
sub rax, 2
jo of_unop_11
jmp cont_unop_11
of_unop_11:
mov rdi, -9
call snek_error
cont_unop_11:
push rax
call fun_isodd
add rsp, 8
jmp if_end_9
if_err_9:
mov rdi, -7
call snek_error
if_end_9:
add rsp, 8
pop rbp
ret
our_code_starts_here:
  mov rdi, 14
push rbp
mov rbp, rsp
mov rax, rdi
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, rdi
push rax
call fun_iseven
add rsp, 8
push rdi
mov rdi, rax
call snek_print
pop rdi
pop rbp
ret
