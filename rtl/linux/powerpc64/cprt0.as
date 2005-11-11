.macro LOAD_64BIT_VAL ra, value 
    lis             \ra,\value@highest
    ori             \ra,\ra,\value@higher
    sldi            \ra,\ra,32
    oris            \ra,\ra,\value@h
    ori             \ra,\ra,\value@l    
.endm

    .section ".rodata"
    .align  3
start_addresses:
    .quad   0 /* was _SDA_BASE_  but not in 64-bit ABI*/
  /*     function descriptors so don't need JUMPTARGET */
    .quad   main_stub
    .quad   __libc_csu_init
    .quad   __libc_csu_fini
    .size   start_adresses, .-start_addresses

    .section ".text"
    .align 2
    .global _start
    .section ".opd","aw"
    .align 3
_start:
    .quad ._start,.TOC.@tocbase,0
    .previous

    .global ._start
._start:
    mr  9,1
    /* Set up an initial stack frame, and clear the LR.  */
    clrrdi  1,1,4
    li  0,0
    stdu    1,-128(1)
    mtlr    0
    std 0,0(1)

 /* put the address of start_addresses in r8...  *
  * PPC64 ABI uses R13 for thread local, so we leave it alone */
        LOAD_64BIT_VAL 8, start_addresses
    
    b   .__libc_start_main
    nop

    .section ".opd","aw"
    .align 3
main_stub:
    .quad .main_stub,.TOC.@tocbase,0
    .previous
    .globl  .main_stub
    .type   .main_stub,@function
.main_stub:
    mflr    0
    std     0,16(1)
    stdu    1,-128(1)

    LOAD_64BIT_VAL 8, operatingsystem_parameter_argc
    stw     3,0(8)

    LOAD_64BIT_VAL 8, operatingsystem_parameter_argv
    std     4,0(8)

    LOAD_64BIT_VAL 8, operatingsystem_parameter_envp
    std     5,0(8)

    LOAD_64BIT_VAL 8, ___fpc_ret
    std 1,0(8)

    bl  .PASCALMAIN
    nop

    b   ._haltproc

    .section ".opd","aw"
    .align 3
_haltproc:
    .quad ._haltproc,.TOC.@tocbase,0
    .previous
    
    .globl  ._haltproc
    .type   ._haltproc,@function
._haltproc:
    LOAD_64BIT_VAL 8, ___fpc_ret
    ld  1,0(8)
    addi    1,1,128
    ld      0,16(1)
    mtlr    0
    blr

#        li      0,1          /* exit call */
#        sc
#        b  ._haltproc

    /* Define a symbol for the first piece of initialized data.  */
    .section ".data"
    .globl  __data_start
__data_start:
data_start:

___fpc_ret:                             /* return address to libc */
        .quad   0

.text
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,8
        .comm operatingsystem_parameter_envp,8
