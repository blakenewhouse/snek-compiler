section .text
extern snek_error, snek_print
global our_code_starts_here
fun_abs:
push rbp
mov rbp, rsp
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
mov rcx, rax
mov rax, [rbp - 8]
; Optimized: skip tag checks for Less (both Num)
cmp rax, rcx
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
mov rcx, rax
cmp rcx, 3
jz cast_num_err_2
cmp rcx, 1
jz cast_num_err_2
jmp cast_num_ok_2
cast_num_err_2:
mov rdi, -19
call snek_error
cast_num_ok_2:
mov rcx, rax
mov rax, [rbp - 8]
; Optimized: skip tag checks for Minus (both Num)
sub rax, rcx
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
mov rsp, rbp
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
mov rcx, rax
mov rax, [rbp - 8]
; Optimized: skip tag checks for Plus (both Num)
add rax, rcx
jo of_ar_4
jmp cont_ar_4
of_ar_4:
mov rdi, -9
call snek_error
cont_ar_4:
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
mov rax, rdi
sub rsp, 8
mov [rsp + 0], rax
call fun_abs
add rsp, 8
add rsp, 8
pop rbp
ret
