section .text
extern snek_error, snek_print
global our_code_starts_here
our_code_starts_here:
push rbp
mov rbp, rsp
sub rsp, 24
; Block:
; Let Binding: a
mov rax, 20
mov [rbp - 8], rax
; Let Binding: b
mov rax, 20
mov [rbp - 16], rax
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, [rbp - 16]
mov rcx, [rbp - 24]
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
add rax, rcx
jo of_ar_1
jmp cont_ar_1
of_ar_1:
mov rdi, -9
call snek_error
cont_ar_1:
; Let Binding: c
mov rax, 3
mov [rbp - 8], rax
; Let Binding: d
mov rax, 38
mov [rbp - 16], rax
; Block:
mov rax, [rbp - 8]
cmp rax, 3
jz isbool_set_true_2
cmp rax, 1
jz isbool_set_true_2
mov rax, 1
jmp isbool_done_2
isbool_set_true_2:
mov rax, 3
isbool_done_2:
mov rax, [rbp - 8]
cmp rax, 3
jz isnum_set_false_3
cmp rax, 1
jz isnum_set_false_3
mov rax, 3
jmp isnum_done_3
isnum_set_false_3:
mov rax, 1
isnum_done_3:
mov rax, [rbp - 16]
cmp rax, 3
jz isbool_set_true_4
cmp rax, 1
jz isbool_set_true_4
mov rax, 1
jmp isbool_done_4
isbool_set_true_4:
mov rax, 3
isbool_done_4:
mov rax, [rbp - 16]
cmp rax, 3
jz isnum_set_false_5
cmp rax, 1
jz isnum_set_false_5
mov rax, 3
jmp isnum_done_5
isnum_set_false_5:
mov rax, 1
isnum_done_5:
; Let Binding: e
mov rax, 1
mov [rbp - 8], rax
; Let Binding: f
mov rax, 24
mov [rbp - 16], rax
; Block:
mov rax, [rbp - 8]
cmp rax, 3
jz isbool_set_true_6
cmp rax, 1
jz isbool_set_true_6
mov rax, 1
jmp isbool_done_6
isbool_set_true_6:
mov rax, 3
isbool_done_6:
cmp rax, 3
jz if_then_7
cmp rax, 1
jz if_else_7
jmp if_err_7
if_then_7:
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, 1
mov rcx, [rbp - 24]
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
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
jmp if_end_7
if_else_7:
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, 2
mov rcx, [rbp - 24]
cmp rax, 3
jz err_ar_9
cmp rax, 1
jz err_ar_9
cmp rcx, 3
jz err_ar_9
cmp rcx, 1
jz err_ar_9
jmp ok_ar_9
err_ar_9:
mov rdi, -1
call snek_error
ok_ar_9:
add rax, rcx
jo of_ar_10
jmp cont_ar_10
of_ar_10:
mov rdi, -9
call snek_error
cont_ar_10:
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
jmp if_end_7
if_err_7:
mov rdi, -7
call snek_error
if_end_7:
mov rax, [rbp - 16]
cmp rax, 3
jz isbool_set_true_11
cmp rax, 1
jz isbool_set_true_11
mov rax, 1
jmp isbool_done_11
isbool_set_true_11:
mov rax, 3
isbool_done_11:
cmp rax, 3
jz if_then_12
cmp rax, 1
jz if_else_12
jmp if_err_12
if_then_12:
mov rax, [rbp - 16]
mov [rbp - 24], rax
mov rax, 1
mov rcx, [rbp - 24]
cmp rax, 3
jz eq_right_bool_13
cmp rax, 1
jz eq_right_bool_13
cmp rcx, 3
jz eq_err_13
cmp rcx, 1
jz eq_err_13
jmp eq_types_ok_13
eq_right_bool_13:
cmp rcx, 3
jz eq_types_ok_13
cmp rcx, 1
jz eq_types_ok_13
jmp eq_err_13
eq_err_13:
mov rdi, -11
call snek_error
eq_types_ok_13:
cmp rax, rcx
mov rax, 0
sete al
shl rax, 1
add rax, 1
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
jmp if_end_12
if_else_12:
mov rax, [rbp - 16]
mov [rbp - 24], rax
mov rax, 4
mov rcx, [rbp - 24]
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
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
jmp if_end_12
if_err_12:
mov rdi, -7
call snek_error
if_end_12:
mov rax, [rbp - 8]
cmp rax, 3
jz isnum_set_false_16
cmp rax, 1
jz isnum_set_false_16
mov rax, 3
jmp isnum_done_16
isnum_set_false_16:
mov rax, 1
isnum_done_16:
cmp rax, 3
jz if_then_17
cmp rax, 1
jz if_else_17
jmp if_err_17
if_then_17:
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, 22
mov rcx, [rbp - 24]
cmp rax, 3
jz err_cmp_18
cmp rax, 1
jz err_cmp_18
cmp rcx, 3
jz err_cmp_18
cmp rcx, 1
jz err_cmp_18
jmp ok_cmp_18
err_cmp_18:
mov rdi, -3
call snek_error
ok_cmp_18:
cmp rcx, rax
mov rax, 0
setg al
shl rax, 1
add rax, 1
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
jmp if_end_17
if_else_17:
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, 3
mov rcx, [rbp - 24]
cmp rax, 3
jz eq_right_bool_19
cmp rax, 1
jz eq_right_bool_19
cmp rcx, 3
jz eq_err_19
cmp rcx, 1
jz eq_err_19
jmp eq_types_ok_19
eq_right_bool_19:
cmp rcx, 3
jz eq_types_ok_19
cmp rcx, 1
jz eq_types_ok_19
jmp eq_err_19
eq_err_19:
mov rdi, -11
call snek_error
eq_types_ok_19:
cmp rax, rcx
mov rax, 0
sete al
shl rax, 1
add rax, 1
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
jmp if_end_17
if_err_17:
mov rdi, -7
call snek_error
if_end_17:
mov rax, [rbp - 16]
cmp rax, 3
jz isnum_set_false_20
cmp rax, 1
jz isnum_set_false_20
mov rax, 3
jmp isnum_done_20
isnum_set_false_20:
mov rax, 1
isnum_done_20:
cmp rax, 3
jz if_then_21
cmp rax, 1
jz if_else_21
jmp if_err_21
if_then_21:
mov rax, [rbp - 16]
mov [rbp - 24], rax
mov rax, 22
mov rcx, [rbp - 24]
cmp rax, 3
jz err_cmp_22
cmp rax, 1
jz err_cmp_22
cmp rcx, 3
jz err_cmp_22
cmp rcx, 1
jz err_cmp_22
jmp ok_cmp_22
err_cmp_22:
mov rdi, -3
call snek_error
ok_cmp_22:
cmp rcx, rax
mov rax, 0
setg al
shl rax, 1
add rax, 1
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
jmp if_end_21
if_else_21:
mov rax, [rbp - 16]
mov [rbp - 24], rax
mov rax, 3
mov rcx, [rbp - 24]
cmp rax, 3
jz eq_right_bool_23
cmp rax, 1
jz eq_right_bool_23
cmp rcx, 3
jz eq_err_23
cmp rcx, 1
jz eq_err_23
jmp eq_types_ok_23
eq_right_bool_23:
cmp rcx, 3
jz eq_types_ok_23
cmp rcx, 1
jz eq_types_ok_23
jmp eq_err_23
eq_err_23:
mov rdi, -11
call snek_error
eq_types_ok_23:
cmp rax, rcx
mov rax, 0
sete al
shl rax, 1
add rax, 1
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
jmp if_end_21
if_err_21:
mov rdi, -7
call snek_error
if_end_21:
; Let Binding: g
mov rax, 3
mov [rbp - 8], rax
mov rax, 3
mov [rbp - 16], rax
mov rax, [rbp - 8]
mov rcx, rax
cmp rcx, 3
jz cast_bool_ok_24
cmp rcx, 1
jz cast_bool_ok_24
cast_bool_err_24:
mov rdi, -19
call snek_error
cast_bool_ok_24:
mov rcx, [rbp - 16]
cmp rax, 3
jz eq_right_bool_25
cmp rax, 1
jz eq_right_bool_25
cmp rcx, 3
jz eq_err_25
cmp rcx, 1
jz eq_err_25
jmp eq_types_ok_25
eq_right_bool_25:
cmp rcx, 3
jz eq_types_ok_25
cmp rcx, 1
jz eq_types_ok_25
jmp eq_err_25
eq_err_25:
mov rdi, -11
call snek_error
eq_types_ok_25:
cmp rax, rcx
mov rax, 0
sete al
shl rax, 1
add rax, 1
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
add rsp, 24
pop rbp
ret
