
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


| Remove $0a character from end of string
    movew  d0,d1
    subqw  #1,d1
    cmpb   #0x0a,a0@(0,d1:w)
    bne    .Lcontt
| Decrement count by one to remove the $0a character
    movew  d1,d0
 .Lcontt:
	 moveb  #0,a0@(0,d0:w)	   | null terminate it
    movew  d0,__ARGC
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





