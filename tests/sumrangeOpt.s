section .text
extern snek_error, snek_print
global our_code_starts_here
fun_sumrange:
push rbp
mov rbp, rsp
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
mov rcx, rax
mov rax, [rbp - 8]
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
mov rax, 2
jmp if_end_2
if_else_2:
mov rax, -2
jmp if_end_2
if_err_2:
mov rdi, -7
call snek_error
if_end_2:
jmp if_end_1
if_else_1:
mov rax, [rbp + 32]
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
jz cast_num_err_4
cmp rcx, 1
jz cast_num_err_4
jmp cast_num_ok_4
cast_num_err_4:
mov rdi, -19
call snek_error
cast_num_ok_4:
mov [rbp - 24], rax
loop_start_5:
mov rax, [rbp - 8]
mov [rbp - 32], rax
mov rax, 0
mov rcx, rax
mov rax, [rbp - 32]
; Optimized: skip tag checks for Greater (both Num)
cmp rax, rcx
mov rax, 0
setg al
shl rax, 1
add rax, 1
cmp rax, 3
jz if_then_6
cmp rax, 1
jz if_else_6
jmp if_err_6
if_then_6:
mov rax, [rbp - 24]
mov [rbp - 32], rax
mov rax, [rbp + 24]
mov rcx, rax
cmp rcx, 3
jz cast_num_err_7
cmp rcx, 1
jz cast_num_err_7
jmp cast_num_ok_7
cast_num_err_7:
mov rdi, -19
call snek_error
cast_num_ok_7:
mov rcx, rax
mov rax, [rbp - 32]
; Optimized: skip tag checks for GreaterEqual (both Num)
cmp rax, rcx
mov rax, 0
setge al
shl rax, 1
add rax, 1
jmp if_end_6
if_else_6:
mov rax, [rbp - 24]
mov [rbp - 32], rax
mov rax, [rbp + 24]
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
mov rcx, rax
mov rax, [rbp - 32]
; Optimized: skip tag checks for LessEqual (both Num)
cmp rax, rcx
mov rax, 0
setle al
shl rax, 1
add rax, 1
jmp if_end_6
if_err_6:
mov rdi, -7
call snek_error
if_end_6:
cmp rax, 3
jz if_then_9
cmp rax, 1
jz if_else_9
jmp if_err_9
if_then_9:
; Break Statement:
mov rax, [rbp - 16]
jmp loop_end_5
jmp if_end_9
if_else_9:
; Block:
; Set! Change: res
mov rax, [rbp - 16]
mov [rbp - 32], rax
mov rax, [rbp - 24]
mov rcx, rax
mov rax, [rbp - 32]
; Optimized: skip tag checks for Plus (both Num)
add rax, rcx
jo of_ar_10
jmp cont_ar_10
of_ar_10:
mov rdi, -9
call snek_error
cont_ar_10:
; res binded to be equal to above
mov [rbp - 16], rax
; Set! Change: curr
mov rax, [rbp - 24]
mov [rbp - 32], rax
mov rax, [rbp - 8]
mov rcx, rax
mov rax, [rbp - 32]
; Optimized: skip tag checks for Plus (both Num)
add rax, rcx
jo of_ar_11
jmp cont_ar_11
of_ar_11:
mov rdi, -9
call snek_error
cont_ar_11:
; curr binded to be equal to above
mov [rbp - 24], rax
jmp if_end_9
if_err_9:
mov rdi, -7
call snek_error
if_end_9:
jmp loop_start_5
loop_end_5:
mov rsp, rbp
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
; Block:
mov rax, 2
sub rsp, 8
mov [rsp + 0], rax
mov rax, 20
sub rsp, 8
mov [rsp + 0], rax
mov rax, 6
sub rsp, 8
mov [rsp + 0], rax
call fun_sumrange
add rsp, 24
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, 1
sub rsp, 8
mov [rsp + 0], rax
mov rax, 6
sub rsp, 8
mov [rsp + 0], rax
mov rax, 20
sub rsp, 8
mov [rsp + 0], rax
call fun_sumrange
add rsp, 24
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, -2
sub rsp, 8
mov [rsp + 0], rax
mov rax, 6
sub rsp, 8
mov [rsp + 0], rax
mov rax, 20
sub rsp, 8
mov [rsp + 0], rax
call fun_sumrange
add rsp, 24
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, 2
sub rsp, 8
mov [rsp + 0], rax
mov rax, 1
sub rsp, 8
mov [rsp + 0], rax
mov rax, 3
sub rsp, 8
mov [rsp + 0], rax
call fun_sumrange
add rsp, 24
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
pop rbp
ret
