.file   "dllprt0.as"
.text
        .globl  _startlib
        .type   _startlib,#function
_startlib:
        .globl  FPC_SHARED_LIB_START
        .type   FPC_SHARED_LIB_START,#function
FPC_SHARED_LIB_START:
        mov ip, sp
        push {fp, ip, lr, pc}
        sub fp, ip, #4
        sub sp, sp, #40

        /* load argc */
        mov a1, ip

        /* load and save a copy of argc  */
        ldr a2, [a1]
        ldr ip, =operatingsystem_parameter_argc
        str a2, [ip]

        /* calc argv and store */
        add a1, a1, #4
        ldr ip, =operatingsystem_parameter_argv
        str a1, [ip]

        /* calc envp and store */
        add a2, a2, #1
        add a2, a1, a2, lsl #2

        ldr ip, =operatingsystem_parameter_envp
        str a2, [ip]

        /* save initial stackpointer */
        ldr ip, =__stklen
        str sp, [ip]

        /* call main and exit normally */
        bl PASCALMAIN
        ldmdb fp, {fp, sp, pc}

        .globl  _haltproc
        .type   _haltproc,#function
_haltproc:
        /* r0 contains exitcode */
        swi 0x900001
        b _haltproc

        .globl  _haltproc_eabi
        .type   _haltproc_eabi,#function
_haltproc_eabi:
        /* r0 contains exitcode */
        mov r7,#248
        swi 0x0
        b _haltproc_eabi

.data

        .type operatingsystem_parameters,#object
        .size operatingsystem_parameters,12
operatingsystem_parameters:
        .skip 3*4
        .global operatingsystem_parameter_envp
        .global operatingsystem_parameter_argc
        .global operatingsystem_parameter_argv
        .set operatingsystem_parameter_envp,operatingsystem_parameters+0
        .set operatingsystem_parameter_argc,operatingsystem_parameters+4
        .set operatingsystem_parameter_argv,operatingsystem_parameters+8

.bss

        .comm __stkptr,4

