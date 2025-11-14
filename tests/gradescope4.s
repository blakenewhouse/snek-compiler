section .text
extern snek_error, snek_print
global our_code_starts_here
fun_max2:
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, [rbp + 24]
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
setg al
shl rax, 1
add rax, 1
cmp rax, 3
jz if_then_1
cmp rax, 1
jz if_else_1
jmp if_err_1
if_then_1:
mov rax, [rbp + 16]
jmp if_end_1
if_else_1:
mov rax, [rbp + 24]
jmp if_end_1
if_err_1:
mov rdi, -7
call snek_error
if_end_1:
add rsp, 8
pop rbp
ret
fun_max3:
push rbp
mov rbp, rsp
mov rax, [rbp + 32]
push rax
mov rax, [rbp + 24]
push rax
mov rax, [rbp + 16]
push rax
call fun_max2
add rsp, 16
push rax
call fun_max2
add rsp, 16
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, 4
push rax
mov rax, 18
push rax
mov rax, 10
push rax
call fun_max3
add rsp, 24
push rdi
mov rdi, rax
call snek_print
pop rdi
pop rbp
ret
