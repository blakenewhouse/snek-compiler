section .text
extern snek_error, snek_print
global our_code_starts_here
fun_f:
push rbp
mov rbp, rsp
mov rax, [rbp + 32]
push rax
call fun_abs
add rsp, 8
push rax
mov rax, [rbp + 24]
push rax
call fun_abs
add rsp, 8
push rax
mov rax, [rbp + 16]
push rax
call fun_abs
add rsp, 8
push rax
call fun_mult
add rsp, 24
pop rbp
ret
fun_abs:
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
mov rax, -2
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
sar rax, 1
sar rcx, 1
imul rax, rcx
shl rax, 1
jo of_ar_3
jmp cont_ar_3
of_ar_3:
mov rdi, -9
call snek_error
cont_ar_3:
jmp if_end_1
if_else_1:
mov rax, [rbp + 16]
jmp if_end_1
if_err_1:
mov rdi, -7
call snek_error
if_end_1:
add rsp, 8
pop rbp
ret
fun_mult:
push rbp
mov rbp, rsp
sub rsp, 16
; Let Binding: t1
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, [rbp + 24]
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
sar rax, 1
sar rcx, 1
imul rax, rcx
shl rax, 1
jo of_ar_5
jmp cont_ar_5
of_ar_5:
mov rdi, -9
call snek_error
cont_ar_5:
mov [rbp - 8], rax
mov rax, [rbp - 8]
mov [rbp - 16], rax
mov rax, [rbp + 32]
mov rcx, [rbp - 16]
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
sar rax, 1
sar rcx, 1
imul rax, rcx
shl rax, 1
jo of_ar_7
jmp cont_ar_7
of_ar_7:
mov rdi, -9
call snek_error
cont_ar_7:
add rsp, 16
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
; Block:
mov rax, 6
push rax
mov rax, 4
push rax
mov rax, 2
push rax
call fun_f
add rsp, 24
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, -6
push rax
mov rax, -4
push rax
mov rax, -2
push rax
call fun_f
add rsp, 24
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, 6
push rax
mov rax, -4
push rax
mov rax, 2
push rax
call fun_f
add rsp, 24
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
pop rbp
ret
