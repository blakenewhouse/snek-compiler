section .text
extern snek_error, snek_print
global our_code_starts_here
fun_sum_up_to:
push rbp
mov rbp, rsp
sub rsp, 24
// Let Binding: i
mov rax, 0
mov [rbp - 8], rax
// Let Binding: acc
mov rax, 0
mov [rbp - 16], rax
loop_start_0:
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, [rbp + 16]
mov rcx, [rbp - 24]
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
setg al
shl rax, 1
add rax, 1
cmp rax, 3
jz if_then_2
cmp rax, 1
jz if_else_2
jmp if_err_2
if_then_2:
// Break Statement:
mov rax, [rbp - 16]
jmp loop_end_0
jmp if_end_2
if_else_2:
// Block:
// Set! Change: acc
mov rax, [rbp - 16]
mov [rbp - 24], rax
mov rax, [rbp - 8]
mov rcx, [rbp - 24]
cmp rax, 3
jz err_ar_3
cmp rax, 1
jz err_ar_3
cmp rcx, 3
jz err_ar_3
cmp rcx, 1
jz err_ar_3
jmp ok_ar_3
err_ar_3:
mov rdi, -1
call snek_error
ok_ar_3:
add rax, rcx
jo of_ar_4
jmp cont_ar_4
of_ar_4:
mov rdi, -9
call snek_error
cont_ar_4:
// acc binded to be equal to above
mov [rbp - 16], rax
// Set! Change: i
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, 2
mov rcx, [rbp - 24]
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
// i binded to be equal to above
mov [rbp - 8], rax
jmp if_end_2
if_err_2:
mov rdi, -7
call snek_error
if_end_2:
jmp loop_start_0
loop_end_0:
add rsp, 24
pop rbp
ret
fun_factorial:
push rbp
mov rbp, rsp
sub rsp, 24
// Let Binding: i
mov rax, 2
mov [rbp - 8], rax
// Let Binding: acc
mov rax, 2
mov [rbp - 16], rax
loop_start_7:
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, [rbp + 16]
mov rcx, [rbp - 24]
cmp rax, 3
jz err_cmp_8
cmp rax, 1
jz err_cmp_8
cmp rcx, 3
jz err_cmp_8
cmp rcx, 1
jz err_cmp_8
jmp ok_cmp_8
err_cmp_8:
mov rdi, -3
call snek_error
ok_cmp_8:
cmp rcx, rax
mov rax, 0
setg al
shl rax, 1
add rax, 1
cmp rax, 3
jz if_then_9
cmp rax, 1
jz if_else_9
jmp if_err_9
if_then_9:
// Break Statement:
mov rax, [rbp - 16]
jmp loop_end_7
jmp if_end_9
if_else_9:
// Block:
// Set! Change: acc
mov rax, [rbp - 16]
mov [rbp - 24], rax
mov rax, [rbp - 8]
mov rcx, [rbp - 24]
cmp rax, 3
jz err_ar_10
cmp rax, 1
jz err_ar_10
cmp rcx, 3
jz err_ar_10
cmp rcx, 1
jz err_ar_10
jmp ok_ar_10
err_ar_10:
mov rdi, -1
call snek_error
ok_ar_10:
sar rax, 1
sar rcx, 1
imul rax, rcx
shl rax, 1
jo of_ar_11
jmp cont_ar_11
of_ar_11:
mov rdi, -9
call snek_error
cont_ar_11:
// acc binded to be equal to above
mov [rbp - 16], rax
// Set! Change: i
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, 2
mov rcx, [rbp - 24]
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
// i binded to be equal to above
mov [rbp - 8], rax
jmp if_end_9
if_err_9:
mov rdi, -7
call snek_error
if_end_9:
jmp loop_start_7
loop_end_7:
add rsp, 24
pop rbp
ret
fun_print_table:
push rbp
mov rbp, rsp
sub rsp, 16
// Let Binding: i
mov rax, 2
mov [rbp - 8], rax
loop_start_14:
mov rax, [rbp - 8]
mov [rbp - 16], rax
mov rax, [rbp + 16]
mov rcx, [rbp - 16]
cmp rax, 3
jz err_cmp_15
cmp rax, 1
jz err_cmp_15
cmp rcx, 3
jz err_cmp_15
cmp rcx, 1
jz err_cmp_15
jmp ok_cmp_15
err_cmp_15:
mov rdi, -3
call snek_error
ok_cmp_15:
cmp rcx, rax
mov rax, 0
setg al
shl rax, 1
add rax, 1
cmp rax, 3
jz if_then_16
cmp rax, 1
jz if_else_16
jmp if_err_16
if_then_16:
// Break Statement:
mov rax, [rbp - 8]
jmp loop_end_14
jmp if_end_16
if_else_16:
// Block:
mov rax, [rbp - 8]
push rax
call fun_sum_up_to
add rsp, 8
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, [rbp - 8]
push rax
call fun_factorial
add rsp, 8
push rdi
mov rdi, rax
call snek_print
pop rdi
// Set! Change: i
mov rax, [rbp - 8]
mov [rbp - 16], rax
mov rax, 2
mov rcx, [rbp - 16]
cmp rax, 3
jz err_ar_17
cmp rax, 1
jz err_ar_17
cmp rcx, 3
jz err_ar_17
cmp rcx, 1
jz err_ar_17
jmp ok_ar_17
err_ar_17:
mov rdi, -1
call snek_error
ok_ar_17:
add rax, rcx
jo of_ar_18
jmp cont_ar_18
of_ar_18:
mov rdi, -9
call snek_error
cont_ar_18:
// i binded to be equal to above
mov [rbp - 8], rax
jmp if_end_16
if_err_16:
mov rdi, -7
call snek_error
if_end_16:
jmp loop_start_14
loop_end_14:
add rsp, 16
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, 14
push rax
call fun_print_table
add rsp, 8
pop rbp
ret
