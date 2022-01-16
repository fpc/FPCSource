/*
  This file is part of the Free Pascal run time library.
  Copyright (c) 2016 by Marcus Sackrow

  Startup code for AROS/ARM RTL

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY;without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  r0 = argc
  r1 = argv
  r2 = ExecBase
  */

  .text
  .globl _start
  .type _start,#function
_start:
  # save registers
  push {r0-r12,r14}
  # save the back jump into OS
  ldr ip,=_Backjump
  str lr,[ip]

  /* ExecBase */
  ldr ip,=_ExecBase
  str r2, [ip]  /* ExecBase seems to be in r2 ;-)*/

  /* Save initial stackpointer*/
  ldr ip,=__stkptr
  str sp,[ip]

  # jump back -> exit
  ldr ip,=_exit
  ldr lr,[ip]

  /* check for stack extent */
  /* FindTask(nil) */
  # execbase to r1
  ldr r1,=_ExecBase
  ldr r1,[r1]
  # nil to r0
  mov r0,#0
  # caluclate offset
  ldr r12,[r1, #-196]
  # do it
  blx r12

  /* get data from PTask splower r0, spupper r1 */
  mov r1, r0
  ldr r0, [r0, #60]
  ldr r1, [r1, #64]
  /* sub spUpper- spLower */
  sub r0,r1,r0
  /* compre with stacklen */
  ldr r1,=__stklen
  ldr r1,[r1]
  /* if OS stack bigger jump to nostack -> no stackswap needed */
  cmp r0, r1
  bge _nostack

/* alloc a new stack and swap to it */
_withstack:
  /* AllocVec(__stklen, MEMF_ANY) */
  # execbase to r2
  ldr r2,=_ExecBase
  ldr r2,[r2]
  # MEMF_ANY (0) to r1
  mov r1,#0
  # stacksize to r0
  ldr r0,=__stklen
  ldr r0,[r0]
  # do the call
  ldr r12,[r2, #-456]
  blx r12

  # save the Pointer to StackAreaPtr
  ldr ip,=StackAreaPtr
  str r0,[ip]
  # check if we got some Memory -> else byebye
  cmp r0, #0
  beq _exit

  /* Prepare StackSwapStruct */
  ldr ip,=StackSwapStruct
  str r0,[ip]      /* stk_Lower */
  # get stacklen
  ldr r1,=__stklen
  ldr r1,[r1]
  # add StackLen to Lower Stack
  add r0,r1,r0
  # put upper stack swap
  str r0,[ip, #4]  /* stk_Upper */
  # start pointer
  str r0,[ip, #8]  /* stk_Pointer */

  /* NewStackSwap(StackSwapStruct, _runpascal, StackSwapArgs) */
  # 4. ExecBase
  ldr r3,=_ExecBase
  ldr r3,[r3]
  # 3. StackSwapArgs
  ldr r2,=StackSwapArgs
  # 2. funcptr
  ldr r1,=_runpascal
  # 1. StackSwapStruct
  ldr r0,=StackSwapStruct
  # do the call
  ldr r12,[r3, #-536]
  blx r12

  # after Stackswap we just to _afterstackswap to leave the program
  bl _afterstackswap

/* NewStackswap will jump here */
_runpascal:
  # save registers on the new stack
  push {r0-r12,lr}
  # save the stackswapped stackptr to
  ldr ip,=__stkptr1
  str sp,[ip]

/* Main Program for both cases */
_nostack:
  # call the Pascal Main
  bl PASCALMAIN
  # seems never reach this point

/* haltproc is called by Pascal */
  .globl  _haltproc
  .type   _haltproc,#function
_haltproc:
  # Check if StackAreaPtr not set -> no need to free, can directly exit
  ldr ip,=StackAreaPtr
  ldr r0,[ip]
  cmp r0, #0
  beq _exit

  # StackAreaPtr is assigned, so we did the Stack Swap and must
  # swap back and free the memory

  # get back the stackswaped stack and get back all registers and jump back
  # to stackswap -> leave the Stack
  ldr ip,=__stkptr1
  ldr sp,[ip]
  pop {r0-r12,lr}
  bx lr

/* jumps here after Stackswap leaving -> with original stack again*/
_afterstackswap:
  # get StackAreaPtr
  ldr ip,=StackAreaPtr
  ldr r0,[ip]

  # freeVec(StackAreaPtr)
  #execbase to r1
  ldr r1,=_ExecBase
  ldr r1,[r1]
  #calc FreeVec offset
  ldr r12,[r1, #-460]
  blx r12

/* jumps here on error or if no stackswap was done */
_exit:
  # get back the original OS Stack
  ldr ip,=__stkptr
  ldr sp,[ip]

  # restore registers
  pop {r0-r12,r14}

  # exitcode should be r0
  ldr ip,=operatingsystem_result
  ldr r0,[ip]

  # jump back to OS
  ldr ip,=_Backjump
  ldr lr,[ip]
  bx lr

  /* Define a symbol for the first piece of initialized data.  */
  .data
  .globl __data_start
__data_start:
  .long 0
  .weak data_start
  data_start = __data_start

.bss
  .comm __stkptr,4
  .comm __stkptr1,4
  .comm _ExecBase,4
  .comm _Backjump,4
  .comm StackAreaPtr,4
  .comm StackSwapStruct, 12
  .comm StackSwapArgs, 32
