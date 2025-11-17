section .text
extern snek_error, snek_print
global our_code_starts_here
fun_f:
push rbp
mov rbp, rsp
sub rsp, 16
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, [rbp + 16]
mov rcx, [rbp - 8]
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
sar rax, 1
sar rcx, 1
imul rax, rcx
shl rax, 1
jo of_ar_1
jmp cont_ar_1
of_ar_1:
mov rdi, -9
call snek_error
cont_ar_1:
mov [rbp - 8], rax
mov rax, [rbp + 24]
mov [rbp - 16], rax
mov rax, [rbp + 24]
mov rcx, [rbp - 16]
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
add rax, rcx
jo of_ar_5
jmp cont_ar_5
of_ar_5:
mov rdi, -9
call snek_error
cont_ar_5:
add rsp, 16
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, 8
push rax
mov rax, 6
push rax
call fun_f
add rsp, 16
pop rbp
ret
