section .text
extern snek_error, snek_print
global our_code_starts_here
fun_get_number:
push rbp
mov rbp, rsp
mov rax, 84
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
call fun_get_number
pop rbp
ret
