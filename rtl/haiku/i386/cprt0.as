       .file   "cprt0.s"
.data
        .align 4
default_environ:
        .long 0
.text
.globl _start
        .type    _start,@function
_start:
        pushl %ebp
        movl %esp,%ebp
        subl $4,%esp
        pushl %ebx
        call .L6
.L6:
        popl %ebx
        addl $_GLOBAL_OFFSET_TABLE_+[.-.L6],%ebx
        movl argv_save@GOT(%ebx),%eax
        movl 12(%ebp),%edi
        movl %edi,(%eax)
        movl environ@GOT(%ebx),%eax
        movl 16(%ebp),%esi
        movl %esi,(%eax)
        test %esi,%esi
        jnz .L4
        movl environ@GOT(%ebx),%eax
        movl %ebx,%ecx
        addl $default_environ@GOTOFF,%ecx
        movl %ecx,%edx
        movl %edx,(%eax)
.L4:
/*      movl %fs:0x4,%eax   this doesn't work on BeOS 4.0, let's use find_thread instead */
        pushl $0x0
        call find_thread
        movl __main_thread_id@GOT(%ebx),%edx
        movl %eax,(%edx)
        pushl %esi
        pushl %edi
        movl 8(%ebp),%eax
        pushl %eax
        call _init_c_library_
        call _call_init_routines_
        movl 8(%ebp),%eax
        movl %eax,operatingsystem_parameter_argc
        movl %edi,operatingsystem_parameter_argv
        movl %esi,operatingsystem_parameter_envp        
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
	
