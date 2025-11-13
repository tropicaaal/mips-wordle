.data


.text

.globl main

main:


exit:
    li $v0, 10
    syscall

# MARK: Procedures

gfx_draw_rect:
    jr ra

gfx_draw_char:
    jr ra

gfx_draw_frame:
    jr ra