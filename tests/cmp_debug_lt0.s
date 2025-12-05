section .text
extern snek_error, snek_print
global our_code_starts_here
our_code_starts_here:
  mov rdi, -2
push rbp
mov rbp, rsp
sub rsp, 8
mov rax, rdi
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
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
add rsp, 8
pop rbp
ret
