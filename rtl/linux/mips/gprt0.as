.include "mips/cprt0.as"

        .option pic2
        .text
        .set  nomips16
        .set  noreorder
        .globl __gmon_start__
        .type  __gmon_start__,@function
__gmon_start__:
        .ent __gmon_start__

        .frame  $sp,32,$ra
        .mask   0x80000000,-4
        .fmask  0x00000000,0
        .cpload $25
        addiu   $sp,$sp,-32
        sw      $ra,28($sp)
        .cprestore 16
        lui     $v0,%hi(called)
        lw      $v1,%lo(called)($v0)
        bne     $v1,$zero,10f

        lw      $a0,%got(__start)($gp)
        lw      $a1,%got(etext)($gp)
        li      $v1,1
        lw      $t9,%call16(__monstartup)($gp)
        jalr    $t9
        sw      $v1,%lo(called)($v0)                  /* in delay slot */
        lw      $gp,16($sp)
        lw      $a0,%got(_mcleanup)($gp)
        lw      $t9,%call16(atexit)($gp)
        jalr    $t9
        nop
10:
        lw      $ra,28($sp)
        jr      $ra
        addiu   $sp,$sp,32
        .end    __gmon_start__
        .size   __gmon_start__,.-__gmon_start__

        .bss
called: .space 4



