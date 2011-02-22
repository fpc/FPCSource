.file   "dllprt0.as"
.text
        .globl  _startlib
        .type   _startlib,#function
_startlib:
        .globl  FPC_SHARED_LIB_START
        .type   FPC_SHARED_LIB_START,#function
FPC_SHARED_LIB_START:
        /* Clear the frame pointer since this is the outermost frame */
        mov fp, #0
        ldmia sp!, {a2}

        /* pop argc off the stack and save a pointer to argv */
        ldr ip,=operatingsystem_parameter_argc
        ldr a3,=operatingsystem_parameter_argv
        str a2,[ip]

        /* calc envp */
        add a2,a2,#1
        add a2,sp,a2,lsl #2
        ldr ip,=operatingsystem_parameter_envp

        str sp,[a3]
        str a2,[ip]

        /* save initial stackpointer */
        ldr ip,=__stklen
        str sp,[ip]
        /* align sp again to 8 byte boundary, needed by eabi */
        sub sp,sp,#4

        /* let the libc call main and exit with its return code */
        bl PASCALMAIN

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

