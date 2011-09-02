//Startup code for WIN32 port of Free Pascal
//Written by P.Ozerski 1998
// modified by Pierre Muller
     .text
     .globl _mainCRTStartup
_mainCRTStartup:
     movb   $1,operatingsystem_isconsole
     jmp    _start

     .globl _WinMainCRTStartup
_WinMainCRTStartup:
     movb   $0,operatingsystem_isconsole
_start:
     subl   $0x8,%esp
     andl   $0xfffffff0,%esp
     push   $_cmain
     call   _cygwin_crt0

     .globl _cmain
_cmain:
     subl   $0x8,%esp
     andl   $0xfffffff0,%esp

     call    __gmon_start__
     call   ___main

     movl   %esp,__stkptr
     call   _FPC_EXE_Entry
     ret

        .globl  __gmon_start__
__gmon_start__:
        pushl   %ebp
        movl    __monstarted,%eax
        leal    0x1(%eax),%edx
        movl    %esp,%ebp
        movl    %edx,__monstarted
        testl   %eax,%eax
        jnz     .Lnomonstart
        pushl   $etext                  /* Initialize gmon */
        pushl   $_cmain
        call    _monstartup
        addl    $8,%esp
.Lnomonstart:
        movl   %ebp,%esp
        popl   %ebp
        ret

     .globl asm_exit
asm_exit:
    pushl   %eax
    call    __mcleanup
    popl    %eax
    pushl   %eax
    call    exitprocess

.text
.globl	exitprocess
exitprocess:
	jmp	*.L10
	.balign 4,144

.text
	.balign 4,144

.section .idata$2
	.rva	.L7
	.long	0,0
	.rva	.L6
	.rva	.L8

.section .idata$4
.L7:
	.rva	.L9
	.long	0

.section .idata$5
.L8:


.section .idata$5
.L10:
	.rva	.L9
	.long	0

.section .idata$6
.L9:
	.short	0
	.ascii	"ExitProcess\000"
	.balign 2,0

.section .idata$7
.L6:
	.ascii	"kernel32.dll\000"

.bss
    .lcomm __monstarted,4
    .comm   __stkptr,4
