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
jz eq_right_bool_0
cmp rax, 1
jz eq_right_bool_0
cmp rcx, 3
jz eq_err_0
cmp rcx, 1
jz eq_err_0
jmp eq_types_ok_0
eq_right_bool_0:
cmp rcx, 3
jz eq_types_ok_0
cmp rcx, 1
jz eq_types_ok_0
jmp eq_err_0
eq_err_0:
mov rdi, -11
call snek_error
eq_types_ok_0:
cmp rax, rcx
mov rax, 0
sete al
shl rax, 1
add rax, 1
cmp rax, 3
jz if_then_1
cmp rax, 1
jz if_else_1
jmp if_err_1
if_then_1:
mov rax, 1
jmp if_end_1
if_else_1:
mov rax, [rbp + 16]
cmp rax, 3
jz err_unop_2
cmp rax, 1
jz err_unop_2
jmp ok_unop_2
err_unop_2:
mov rdi, -1
call snek_error
ok_unop_2:
sub rax, 2
jo of_unop_3
jmp cont_unop_3
of_unop_3:
mov rdi, -9
call snek_error
cont_unop_3:
push rax
call fun_iseven
add rsp, 8
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
mov rax, 3
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
call fun_isodd
add rsp, 8
jmp if_end_5
if_err_5:
mov rdi, -7
call snek_error
if_end_5:
add rsp, 8
pop rbp
ret
our_code_starts_here:
  mov rdi, 10
push rbp
mov rbp, rsp
; Block:
mov rax, rdi
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, rdi
mov rcx, rax
cmp rcx, 3
jz cast_num_err_8
cmp rcx, 1
jz cast_num_err_8
jmp cast_num_ok_8
cast_num_err_8:
mov rdi, -19
call snek_error
cast_num_ok_8:
push rax
call fun_iseven
add rsp, 8
push rdi
mov rdi, rax
call snek_print
pop rdi
pop rbp
ret
