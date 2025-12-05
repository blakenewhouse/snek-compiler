section .text
extern snek_error, snek_print
global our_code_starts_here
fun_rule1:
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
mov rax, 84
jmp if_end_1
if_else_1:
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 20
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
if_err_1:
mov rdi, -7
call snek_error
if_end_1:
add rsp, 8
pop rbp
ret
fun_rule2:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
cmp rax, 3
jz isbool_set_true_4
cmp rax, 1
jz isbool_set_true_4
mov rax, 1
jmp isbool_done_4
isbool_set_true_4:
mov rax, 3
isbool_done_4:
cmp rax, 3
jz if_then_5
cmp rax, 1
jz if_else_5
jmp if_err_5
if_then_5:
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 3
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
jmp if_end_5
if_else_5:
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 20
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
jmp if_end_5
if_err_5:
mov rdi, -7
call snek_error
if_end_5:
add rsp, 8
pop rbp
ret
fun_rule3:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
cmp rax, 3
jz isnum_set_false_10
cmp rax, 1
jz isnum_set_false_10
mov rax, 3
jmp isnum_done_10
isnum_set_false_10:
mov rax, 1
isnum_done_10:
cmp rax, 3
jz if_then_11
cmp rax, 1
jz if_else_11
jmp if_err_11
if_then_11:
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 20
mov rcx, [rbp - 8]
cmp rax, 3
jz err_ar_12
cmp rax, 1
jz err_ar_12
cmp rcx, 3
jz err_ar_12
cmp rcx, 1
jz err_ar_12
jmp ok_ar_12
err_ar_12:
mov rdi, -1
call snek_error
ok_ar_12:
add rax, rcx
jo of_ar_13
jmp cont_ar_13
of_ar_13:
mov rdi, -9
call snek_error
cont_ar_13:
jmp if_end_11
if_else_11:
mov rax, 84
jmp if_end_11
if_err_11:
mov rdi, -7
call snek_error
if_end_11:
add rsp, 8
pop rbp
ret
fun_rule4:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
cmp rax, 3
jz isnum_set_false_14
cmp rax, 1
jz isnum_set_false_14
mov rax, 3
jmp isnum_done_14
isnum_set_false_14:
mov rax, 1
isnum_done_14:
cmp rax, 3
jz if_then_15
cmp rax, 1
jz if_else_15
jmp if_err_15
if_then_15:
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 20
mov rcx, [rbp - 8]
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
jmp if_end_15
if_else_15:
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 3
mov rcx, [rbp - 8]
cmp rax, 3
jz err_ar_18
cmp rax, 1
jz err_ar_18
cmp rcx, 3
jz err_ar_18
cmp rcx, 1
jz err_ar_18
jmp ok_ar_18
err_ar_18:
mov rdi, -1
call snek_error
ok_ar_18:
add rax, rcx
jo of_ar_19
jmp cont_ar_19
of_ar_19:
mov rdi, -9
call snek_error
cont_ar_19:
jmp if_end_15
if_err_15:
mov rdi, -7
call snek_error
if_end_15:
add rsp, 8
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
; Block:
mov rax, 3
push rax
call fun_rule1
add rsp, 8
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, 10
push rax
call fun_rule2
add rsp, 8
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, 1
push rax
call fun_rule3
add rsp, 8
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, 14
push rax
call fun_rule4
add rsp, 8
pop rbp
ret
