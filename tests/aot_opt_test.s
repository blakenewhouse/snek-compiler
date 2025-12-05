section .text
extern snek_error, snek_print
global our_code_starts_here
fun_test_optimizations:
push rbp
mov rbp, rsp
; Block:
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 10
mov rcx, rax
mov rax, [rbp - 8]
; Optimized: skip tag checks for Plus (both Num)
add rax, rcx
jo of_binop_0
jmp cont_binop_0
of_binop_0:
mov rdi, -9
call snek_error
cont_binop_0:
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
; Optimized: isnum with Num => true
mov rax, 3
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
; Optimized: isbool with Bool => true
mov rax, 3
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
; Optimized: isnum with Num - skip condition
mov rax, [rbp + 16]
mov [rbp - 8], rax
mov rax, 20
mov rcx, rax
mov rax, [rbp - 8]
; Optimized: skip tag checks for Plus (both Num)
add rax, rcx
jo of_binop_1
jmp cont_binop_1
of_binop_1:
mov rdi, -9
call snek_error
cont_binop_1:
mov rsp, rbp
pop rbp
ret
our_code_starts_here:
push rbp
mov rbp, rsp
mov rax, 14
sub rsp, 8
mov [rsp + 0], rax
mov rax, 3
sub rsp, 8
mov [rsp + 0], rax
call fun_test_optimizations
add rsp, 16
mov rsp, rbp
pop rbp
ret
