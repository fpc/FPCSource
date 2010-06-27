.file   "androidprt0.as"
.text

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
