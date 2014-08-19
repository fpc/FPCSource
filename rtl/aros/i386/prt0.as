#
#  This file is part of the Free Pascal run time library.
#  Copyright (c) 2011 by Marcus Sackrow
#
#  Startup code for AROS/i386 RTL
#
#  See the file COPYING.FPC, included in this distribution,
#  for details about the copyright.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY;without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

#AROS Startup Code
#
   .text
   .align 4
   .section .aros.startup, "ax"		
   .globl _start
   .globl start
   .globl _haltproc
   .globl haltproc
_start:
start:

  /* Save the exec library base */
  movl     12(%esp), %ecx
  movl     %ecx, _ExecBase

  /* Save the command line pointer length to CommandLineLen */
  movl     8(%esp),%ecx
  movl     %ecx,CommandLineLen

  /* Save the command line pointer to CommandLine */
  movl     4(%esp),%eax
  movl     %eax,CommandLine

  /* save all register */
  pushal

  /* Save stack pointer for exit() routine */
  movl	   %esp,STKPTR

  call   PASCALMAIN
  /* if returns from here set an empty returncode */
  xorl   %eax, %eax
  pushl  %eax
  pushl  %eax


  /* entry for stop the program*/
_haltproc:
haltproc:

    /* get retun code from stack */
    movl   4(%esp),%eax

    /* save for later use */
    movl   %eax,_returncode

    /* get back my stack */
    movl   STKPTR,%esp

    /* get back all registers */
    popal

    /* reset returncode */
    movl  _returncode, %eax

    /* bye bye */
    ret

  /*----------------------------------------------------*/

    .data
    .global CommandLineLen      # byte length of command line
    .global CommandLine         # comandline as PChar
    .global STKPTR              # Used to terminate the program, initial SP
    .global _ExecBase           # exec library base
    .align 4

_returncode:    .long   0
CommandLine:    .long   0
CommandLineLen: .long   0
STKPTR:         .long   0
_ExecBase:      .long   0

