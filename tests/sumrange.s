section .text
extern snek_error, snek_print
global our_code_starts_here
fun_sumrange:
push rbp
mov rbp, rsp
sub rsp, 32
; Let Binding: step
mov rax, [rbp + 32]
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
mov rax, [rbp + 32]
mov [rbp - 8], rax
mov rax, 3
mov rcx, [rbp - 8]
cmp rax, 3
jz eq_right_bool_2
cmp rax, 1
jz eq_right_bool_2
cmp rcx, 3
jz eq_err_2
cmp rcx, 1
jz eq_err_2
jmp eq_types_ok_2
eq_right_bool_2:
cmp rcx, 3
jz eq_types_ok_2
cmp rcx, 1
jz eq_types_ok_2
jmp eq_err_2
eq_err_2:
mov rdi, -11
call snek_error
eq_types_ok_2:
cmp rax, rcx
mov rax, 0
sete al
shl rax, 1
add rax, 1
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
mov rax, [rbp + 32]
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
; Let Binding: res
mov rax, 0
mov [rbp - 16], rax
; Let Binding: curr
mov rax, [rbp + 16]
mov rcx, rax
cmp rcx, 3
jz cast_num_err_5
cmp rcx, 1
jz cast_num_err_5
jmp cast_num_ok_5
cast_num_err_5:
mov rdi, -19
call snek_error
cast_num_ok_5:
mov [rbp - 24], rax
loop_start_6:
mov rax, [rbp - 8]
mov [rbp - 32], rax
mov rax, 0
mov rcx, [rbp - 32]
cmp rax, 3
jz err_cmp_7
cmp rax, 1
jz err_cmp_7
cmp rcx, 3
jz err_cmp_7
cmp rcx, 1
jz err_cmp_7
jmp ok_cmp_7
err_cmp_7:
mov rdi, -3
call snek_error
ok_cmp_7:
cmp rcx, rax
mov rax, 0
setg al
shl rax, 1
add rax, 1
cmp rax, 3
jz if_then_8
cmp rax, 1
jz if_else_8
jmp if_err_8
if_then_8:
mov rax, [rbp - 24]
mov [rbp - 32], rax
mov rax, [rbp + 24]
mov rcx, rax
cmp rcx, 3
jz cast_num_err_9
cmp rcx, 1
jz cast_num_err_9
jmp cast_num_ok_9
cast_num_err_9:
mov rdi, -19
call snek_error
cast_num_ok_9:
mov rcx, [rbp - 32]
cmp rax, 3
jz err_cmp_10
cmp rax, 1
jz err_cmp_10
cmp rcx, 3
jz err_cmp_10
cmp rcx, 1
jz err_cmp_10
jmp ok_cmp_10
err_cmp_10:
mov rdi, -3
call snek_error
ok_cmp_10:
cmp rcx, rax
mov rax, 0
setge al
shl rax, 1
add rax, 1
jmp if_end_8
if_else_8:
mov rax, [rbp - 24]
mov [rbp - 32], rax
mov rax, [rbp + 24]
mov rcx, rax
cmp rcx, 3
jz cast_num_err_11
cmp rcx, 1
jz cast_num_err_11
jmp cast_num_ok_11
cast_num_err_11:
mov rdi, -19
call snek_error
cast_num_ok_11:
mov rcx, [rbp - 32]
cmp rax, 3
jz err_cmp_12
cmp rax, 1
jz err_cmp_12
cmp rcx, 3
jz err_cmp_12
cmp rcx, 1
jz err_cmp_12
jmp ok_cmp_12
err_cmp_12:
mov rdi, -3
call snek_error
ok_cmp_12:
cmp rcx, rax
mov rax, 0
setle al
shl rax, 1
add rax, 1
jmp if_end_8
if_err_8:
mov rdi, -7
call snek_error
if_end_8:
cmp rax, 3
jz if_then_13
cmp rax, 1
jz if_else_13
jmp if_err_13
if_then_13:
; Break Statement:
mov rax, [rbp - 16]
jmp loop_end_6
jmp if_end_13
if_else_13:
; Block:
; Set! Change: res
mov rax, [rbp - 16]
mov [rbp - 32], rax
mov rax, [rbp - 24]
mov rcx, [rbp - 32]
cmp rax, 3
jz err_ar_14
cmp rax, 1
jz err_ar_14
cmp rcx, 3
jz err_ar_14
cmp rcx, 1
jz err_ar_14
jmp ok_ar_14
err_ar_14:
mov rdi, -1
call snek_error
ok_ar_14:
add rax, rcx
jo of_ar_15
jmp cont_ar_15
of_ar_15:
mov rdi, -9
call snek_error
cont_ar_15:
; res binded to be equal to above
mov [rbp - 16], rax
; Set! Change: curr
mov rax, [rbp - 24]
mov [rbp - 32], rax
mov rax, [rbp - 8]
mov rcx, [rbp - 32]
cmp rax, 3
jz err_ar_16
cmp rax, 1
jz err_ar_16
cmp rcx, 3
jz err_ar_16
cmp rcx, 1
jz err_ar_16
jmp ok_ar_16
err_ar_16:
mov rdi, -1
call snek_error
ok_ar_16:
add rax, rcx
jo of_ar_17
jmp cont_ar_17
of_ar_17:
mov rdi, -9
call snek_error
cont_ar_17:
; curr binded to be equal to above
mov [rbp - 24], rax
jmp if_end_13
if_err_13:
mov rdi, -7
call snek_error
if_end_13:
jmp loop_start_6
loop_end_6:
add rsp, 32
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
; Block:
mov rax, 2
push rax
mov rax, 20
push rax
mov rax, 6
push rax
call fun_sumrange
add rsp, 24
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, 1
push rax
mov rax, 6
push rax
mov rax, 20
push rax
call fun_sumrange
add rsp, 24
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, -2
push rax
mov rax, 6
push rax
mov rax, 20
push rax
call fun_sumrange
add rsp, 24
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, 2
push rax
mov rax, 1
push rax
mov rax, 3
push rax
call fun_sumrange
add rsp, 24
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
pop rbp
ret
