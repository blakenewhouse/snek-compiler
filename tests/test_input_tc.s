section .text
extern snek_error, snek_print
global our_code_starts_here
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, rdi
pop rbp
ret
