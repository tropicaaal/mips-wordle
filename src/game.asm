.data


.text

.globl main

main:

# MARK: Syscalls

# syscall 10: Exit
sys_exit:
    li $v0, 10
    syscall

# syscall 11: Print Character
sys_print_char:
    li $v0, 11
    syscall
    jr ra

# syscall 42: random int range
sys_rand_range:
    li $a0, 101         #0- 100 range
    li $v0, 42          #random int syscall
    syscall             #V0 gets random number
    jr ra

# MARK: Graphics

gfx_draw_rect:
    jr ra

gfx_draw_char:
    jr ra

gfx_draw_frame:
    jr ra