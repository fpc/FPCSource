/*
Startup code for WinCE port of Free Pascal
Written by Yury Sidorov 2005
*/

.section .text
  .balign 4
.globl mainCRTStartup
mainCRTStartup:
.globl _mainCRTStartup
_mainCRTStartup:
  mov r0,#1
  b do_start

.globl WinMainCRTStartup
WinMainCRTStartup:
.globl _WinMainCRTStartup
_WinMainCRTStartup:
  mov r0,#0
do_start:
  ldr r1, _PISCONSOLE
  strb r0,[r1]
  bl _FPC_EXE_Entry

.globl asm_exit
asm_exit:
  eor   r0,r0,r0
  bl	exitthread
  
_PISCONSOLE:
  .long U_SYSTEM_ISCONSOLE

.globl exitthread
exitthread:
	ldr	ip,.L100
	ldr pc,[ip]
.L100:
  .long .L10

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
	.ascii	"ExitThread\000"
	.balign 2,0

.section .idata$7
.L6:
	.ascii	"coredll.dll\000"
