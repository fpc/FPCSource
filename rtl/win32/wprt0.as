//Startup code for WIN32 port of Free Pascal
//Written by P.Ozerski 1998
// modified by Pierre Muller
     .text
     .globl _mainCRTStartup
_mainCRTStartup:
     movb   $1,operatingsystem_isconsole
     movl   %esp,__stkptr
     call   _FPC_EXE_Entry
     .globl _WinMainCRTStartup
_WinMainCRTStartup:
     movb   $0,operatingsystem_isconsole
     movl   %esp,__stkptr
     call   _FPC_EXE_Entry

     .globl asm_exit
asm_exit:
    pushl   %eax
	call	exitprocess

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
    .comm   __stkptr,4
