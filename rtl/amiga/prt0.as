
   .text

   .align 4

   .globl _start
   .globl start
_start:
start:
|	Save stack pointer for exit() routine

	movel	   sp,STKPTR	| save stack pointer
| This was wrong compared to PCQ
|	addl	   #4,STKPTR	| account for this jsr to get to original

|	Save the command line pointer to CommandLine

    movel	a0,__ARGS
    beq    .Ldont_nullit

	 moveb  #0,a0@(0,d0:w)	   | null terminate it
 .Ldont_nullit:

    jsr PASCALMAIN

    movel  STKPTR,sp
    rts

    .data

    .align 4

    .globl __ARGS
 __ARGS:                   | pointer to the arguments
      .long 0
    .globl  __ARGC
 __ARGC:                    | number of arguments
      .word 0
    .globl STKPTR          | Used to terminate the program, initial SP
 STKPTR:
      .long 0





