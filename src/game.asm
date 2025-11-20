.data
imageBuffer:
.space 0x80000   # 512 * 256 * 4 bytes

.text

.globl main

main:
    jal gfx_draw_frame # draw test frame
    j sys_exit

# MARK: Syscalls

# syscall 10: Exit
sys_exit:
    li $v0, 10
    syscall
    jr $ra

# syscall 11: Print Character
sys_print_char:
    li $v0, 11
    syscall
    jr $ra

# syscall 42: random int range
sys_rand_range:
    li $v0, 42          #random int syscall
    syscall             #V0 gets random number
    jr $ra

# MARK: Graphics

# gfx_draw_rect(x, y, width, height, color)
gfx_draw_rect:
    beq $a2, $zero, rtn   # zero width → nothing to draw
    beq $a3, $zero, rtn   # zero height → nothing to draw

    li $t0, 0x00606060   # pixel color

    la $t1, imageBuffer  # base addr of framebuffer

    # Compute x2 = x + width
    add $t5, $a0, $a2

    # Compute y2 = y + height
    add $t6, $a1, $a3

    # Convert x, x2 into byte offsets: x * 4
    sll $a0, $a0, 2       # x1_scaled
    sll $t5, $t5, 2       # x2_scaled

    # Convert y, y2 into byte offsets: y * (512*4) = y << 11
    sll $a1, $a1, 11      # y1_scaled
    sll $t6, $t6, 11      # y2_scaled

    # Compute starting and ending addresses
    addu $t2, $t1, $a1        # base + y1_scaled
    addu $a1, $t2, $a0        # first row, left pixel

    addu $t6, $t6, $t1        # base + y2_scaled
    addu $t6, $t6, $a0        # last row, left pixel of last row

    addu $t2, $t2, $t5        # first row, right pixel end

    li $t4, 2048             # bytes per row = 512*4

    y_loop:
        move $t3, $a1            # t3 = pointer to left pixel of current row

        x_loop:
            sw $t0,0($t3)
            addiu $t3,$t3,4
            bne $t3,$t2,x_loop

            # Move downward one row (left edge)
            addiu $a1,$a1,2048
            # Move downward one row (right edge)
            addiu $t2,$t2,2048

            bne $a1,$t6,y_loop

    rtn:
        jr $ra

gfx_draw_char:
    jr ra

gfx_draw_board:
    # top part
    li $a0, 50       # x
    li $a1, 50       # y
    li $a2, 250      # width
    li $a3, 10       # height
    jal rectangle

    li $s0, 6          # number of rectangles
    li $t8, 7

    li $s3, 50         # y
    li $s4, 10         # width
    li $s5, 50         # height
    li $s6, 50         # x increment
    li $s7, 0	  # other loop counter

    down_loop:
        li $s1, 0         # loop counter
        li $s2, 50        # starting x

    across_loop:
        move $a0, $s2      # x
        move $a1, $s3      # y
        move $a2, $s4      # width
        move $a3, $s5      # height
        jal rectangle

        add $s2, $s2, $s6  # x += 50
        addiu $s1, $s1, 1
        bne $s1, $s0, across_loop

        #bottom part
        li $a0,50       # x
        add $a1, $s3, $s5       # y
        li $a2,260        # width
        li $a3,10        # height
        jal rectangle    
            
        add $s3, $s3, $s5
        addiu $s7, $s7, 1
        bne $s7, $s0, down_loop

gfx_draw_frame:
    jal gfx_draw_board
    jr $ra