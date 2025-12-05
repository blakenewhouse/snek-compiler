section .text
extern snek_error, snek_print
global our_code_starts_here
our_code_starts_here:
push rbp
mov rbp, rsp
sub rsp, 24
; Block:
; Let Binding: a
mov rax, 20
mov [rbp - 8], rax
; Let Binding: b
mov rax, 20
mov [rbp - 16], rax
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, [rbp - 16]
mov rcx, rax
mov rax, [rbp - 24]
; Optimized: skip tag checks for Plus (both Num)
add rax, rcx
jo of_ar_0
jmp cont_ar_0
of_ar_0:
mov rdi, -9
call snek_error
cont_ar_0:
; Let Binding: c
mov rax, 3
mov [rbp - 8], rax
; Let Binding: d
mov rax, 38
mov [rbp - 16], rax
; Block:
; Optimized: isbool with Bool => true
mov rax, 3
; Optimized: isnum with Bool => false
mov rax, 1
; Optimized: isbool with Num => false
mov rax, 1
; Optimized: isnum with Num => true
mov rax, 3
; Let Binding: e
mov rax, 1
mov [rbp - 8], rax
; Let Binding: f
mov rax, 24
mov [rbp - 16], rax
; Block:
; Optimized: isbool with Bool - skip condition
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, 1
mov rcx, rax
mov rax, [rbp - 24]
cmp rax, rcx
mov rax, 0
sete al
shl rax, 1
add rax, 1
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
; Optimized: isbool with Num - skip condition
mov rax, [rbp - 16]
mov [rbp - 24], rax
mov rax, 4
mov rcx, rax
mov rax, [rbp - 24]
; Optimized: skip tag checks for Plus (both Num)
add rax, rcx
jo of_ar_1
jmp cont_ar_1
of_ar_1:
mov rdi, -9
call snek_error
cont_ar_1:
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
; Optimized: isnum with Bool - skip condition
mov rax, [rbp - 8]
mov [rbp - 24], rax
mov rax, 3
mov rcx, rax
mov rax, [rbp - 24]
cmp rax, rcx
mov rax, 0
sete al
shl rax, 1
add rax, 1
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
; Optimized: isnum with Num - skip condition
mov rax, [rbp - 16]
mov [rbp - 24], rax
mov rax, 22
mov rcx, rax
mov rax, [rbp - 24]
; Optimized: skip tag checks for Greater (both Num)
cmp rax, rcx
mov rax, 0
setg al
shl rax, 1
add rax, 1
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
; Let Binding: g
mov rax, 3
mov [rbp - 8], rax
mov rax, 3
mov [rbp - 16], rax
; Optimized: cast Bool to Bool eliminated (subtype)
mov rax, [rbp - 8]
mov rcx, rax
mov rax, [rbp - 16]
cmp rax, rcx
mov rax, 0
sete al
shl rax, 1
add rax, 1
; Print:
push rdi
mov rdi, rax
call snek_print
pop rdi
add rsp, 24
pop rbp
ret
