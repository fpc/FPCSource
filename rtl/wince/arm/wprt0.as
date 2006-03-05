/*
Startup code for WinCE port of Free Pascal
Written by Yury Sidorov, 2005.
*/

.section .text
@ for kernel exception handler, must be directly before ___EH_CODE_START__
__EH_HANDLER__:
	.word _ARM_ExceptionHandler
	.word 0

__EH_CODE_START__:

@ EXE entry points
.globl _mainCRTStartup
_mainCRTStartup:
  mov r3,#1
  b do_start

.globl _WinMainCRTStartup
_WinMainCRTStartup:
  mov r3,#0
do_start:
  ldr r12, _PISCONSOLE
  strb r3,[r12]
  b _FPC_EXE_Entry

@ DLL entry points
.globl _DLLMainCRTStartup
_DLLMainCRTStartup:
  mov r3,#1
  b do_start2

.globl _DLLWinMainCRTStartup
_DLLWinMainCRTStartup:
  mov r3,#0
do_start2:
  ldr r12, _PISCONSOLE
  strb r3,[r12]
  b _FPC_DLLMainStartup

_PISCONSOLE:
  .long U_SYSTEM_ISCONSOLE

@ for kernel exception handler
	.section .pdata
	.word __EH_CODE_START__
@ max 22 bits for number of instructions
	.word 0xc0000002 | (0xFFFFF << 8)
