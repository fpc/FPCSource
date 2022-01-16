       .file   "dllcprt0.as"
.data
        .align 4
default_environ:
        .long 0
.text
.globl initialize_after
        .type    initialize_after,@function
initialize_after:
        .globl FPC_SHARED_LIB_START
        .type FPC_SHARED_LIB_START,@function
FPC_SHARED_LIB_START:
        /* We are in a library if we link something against this code */
        movb $1,operatingsystem_islibrary
        /* Initialize freepascal variables in the shared object so they 
           can be used as expected.
           
           As we link with libroot (our libc), just copy values from the 
           corresponding external variables in the Freepascal ones. 
           They are already initialized by libroot initialization. 
           
           Inspired by /haiku/src/system/glue/start_dyn.c 
           and /haiku/src/system/libroot/libroot_init.c
        */        
        movl __libc_argc,%eax
        movl %eax,operatingsystem_parameter_argc
        movl __libc_argv,%eax
        movl %eax,operatingsystem_parameter_argv
        movl environ,%eax
        movl %eax,operatingsystem_parameter_envp
        xorl %ebp,%ebp
        call PASCALMAIN

.globl  _haltproc
.type   _haltproc,@function
_haltproc:
        call _thread_do_exit_notification
        xorl %ebx,%ebx
    movw operatingsystem_result,%bx
        pushl %ebx
        call exit

.bss
        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4

