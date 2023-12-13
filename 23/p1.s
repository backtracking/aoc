##  Advent of Code 2023, #1
.text
.globl main
        ## %rbx : total sum
        ## %r12 : first
        ## %r13 : last
main:
        push %rbx
        push %r12
        push %r13
        xor  %ebx, %ebx
        xor  %r12d, %r12d
        xor  %r13d, %r13d
        ## new line
1:      imul $10, %r12d
        add  %r13d, %r12d
        add  %r12d, %ebx
        mov  $-1, %r12d
2:      call getchar
        cmp  $-1, %eax
        je   3f
        cmp  $'\n', %eax
        je   1b
        sub  $'0', %eax
        jl   2b
        cmp  $9, %eax
        jg   2b
        ## digit
        cmp  $-1, %r12d
        cmove %eax, %r12d
        mov  %eax, %r13d
        jmp  2b
3:      mov  $Lmsg, %rdi
        mov  %ebx, %esi
        xor  %rax, %rax
        call printf
        xor  %rax, %rax
        pop  %r13
        pop  %r12
        pop  %rbx
        ret
.data
Lmsg:   .string "sum = %d\n"

## Local Variables:
## compile-command: "gcc -no-pie p1.s -o p1 && ./p1"
## End:
