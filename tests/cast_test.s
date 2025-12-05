section .text
extern snek_error, snek_print
global our_code_starts_here
fun_test:
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
mov rsp, rbp
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, 10
sub rsp, 8
mov [rsp + 0], rax
call fun_test
add rsp, 8
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
pop rbp
ret
