|*************************************************************************
|*                                                                       *
|*       DSTART.S        Startup module for Pascal programs using dLibs  *
|*                                                                       *
|*************************************************************************

|*
|* entry points
|*
.globl  __base                  | basepage pointer
.globl  __start                 | startup entry point
.globl  _etext                  | end of text segment
.globl  _edata                  | end of data segment
.globl  _end                    | end of BSS segment (end of program)
.globl  __BREAK                 | location of stack/heap break
.globl  __ARGC                  | number of arguments
.globl  __ARGS                  | argument list pointer
.globl  __envp                  | environment string pointer
.globl  _errno                  | system error number

|
| external references
|
|.globl  __stklen               | Stack size value from C (unsigned long)

|
| useful constants
|
MINSTK          =     16384   | Minimum 16K stack size
MARGIN          =     512     | Minimum memory to return to OS

|
| GEMDOS functions
|
Cconws          =     0x09     | Console write string
Pterm           =     0x4C     | Process terminate (with exit code)
Mshrink         =     0x4A     | Shrink program space

|
| basepage offsets
|
p_hitpa         =     0x04     | top of TPA
p_tbase         =     0x08     | base of text
p_tlen          =     0x0C     | length of text
p_dbase         =     0x10     | base of data
p_dlen          =     0x14     | length of data
p_bbase         =     0x18     | base of BSS
p_blen          =     0x1C     | length of BSS
p_env           =     0x2C     | environment string
p_cmdlin        =     0x80     | command line image

|
| STARTUP ROUTINE (must be first object in link)
|
.text
   .globl start
   .globl _start

__start:
start:
_start:
|
| save initial stack and basepage pointers
|
	movel  sp,a5                   | a5 = initial stack pointer
	movel  sp@(4),a4                | a4 = basepage address
	movel  a4,__base
	movel  a4@(p_tbase),d3
	addl   a4@(p_tlen),d3
	movel  d3,_etext               | end of text segment
	movel  a4@(p_dbase),d3
	addl   a4@(p_dlen),d3
	movel  d3,_edata               | end of data segment
	movel  a4@(p_bbase),d3
	addl   a4@(p_blen),d3
	movel  d3,_end                 | end of BSS (end of program)
	movel  d3,__BREAK;             | set initial _break value
	movel  a4@(p_env),__envp        | save environment pointer
|
| call C function to get command line arguments
|
	lea   a4@(p_cmdlin),a0         | get pointer to command line image
	moveb  a0@+,d0
	extw   d0                     | extract length
|	movew  d0,sp@-                | cmdlen
|	movel  a0,sp@-                | cmdline
|	jsr     __initar              | call _initargs(cmdline, cmdlen)
|	addql  #6,sp
   movew  d0,__ARGC              | save length
   movel  a0,__ARGS              | save pointer to string
|
| calculate free space available to program
|
	movel  __BREAK,d3
	movel  d3,a3                   | a3 = base of free space
	negl   d3
	addl   a4@(p_hitpa),d3
	subl   #MARGIN,d3              | d3 = free space
|
| calculate new stack size (store in d2)
|
| ASSUME 8K STACK FOR THE MOMENT.
 	movel   __stklen,d2            | d2 = _STKSIZ
 	tstl    d2                     | if __STKSIZ is zero
	beq     minimum                |   use MINSTK
	bra     setstk                 |   use __STKSIZ
minimum:
	movel  #MINSTK,d2              |   use MINSTK
|
| check to see if there is enough room for requested stack
|
setstk:
	cmpl   d3,d2
	blt     shrink                  | if (available < requested)
	movel  #stkerr,sp@-
	movew  #Cconws,sp@-
	trap    #1                      |   report a stack error
	addql  #6,sp
	movew  #-39,sp@-
	movew  #Pterm,sp@-
	trap    #1                      |   and return error -39 (ENSMEM)
|
| set up new stack pointer and Mshrink
|
shrink:
	addl   a3,d2                   | new stack = free base + stack size
	movel  d2,sp
	subl   a4,d2                   | keep space = new stack - __base
	movel  d2,sp@-
	movel  a4,sp@-
	clrw   sp@-
	movew  #Mshrink,sp@-
	trap    #1                      | Mshrink(0, _base, keep);
	addl   #12,sp
|
| call C entry point function _main()
|
	jsr     PASCALMAIN               | if _main returns

   movew   #0,sp@-                  | Terminate program normally
   trap    #1

|	movew   d0,sp@(4)                |   insert return value and fall thru



|
| check for stack overflow (done after all OS traps)
|
chkstk:
	cmpl    __BREAK,sp
	bgt     nosweat                 | if (_break > sp)
	movel   #stkovf,sp@-
	movew   #Cconws,sp@-
	trap    #1                      |   report a stack overflow
	addql   #6,sp
	movew   #-1,sp@-
	movew   #Pterm,sp@-
	trap    #1                      |   and return error -1 (ERROR)
nosweat:
	movel   traprtn,sp@-           | else, restore return address
	rts                             | and do a normal return.

|
| this call to _main ensures that it the user's main() function will be
| linked, even if it is in a library.
|
	jsr     PASCALMAIN                   | NO PATH TO THIS STATEMENT
   movew   d0,sp@-
   movew   #0x4c,sp@-
   trap    #1


|
| initialized data space
|
.data
.even
stkerr:                                 | not enough memory for stack
	.ascii   "Not enough memory"
   .byte     0x0d,0x0a,0x00
stkovf:                                 | impending stack overflow
	.ascii   "Stack overflow"
   .byte     0x0d,0x0a,0x00
_errno:                                 | system error number
	.word   0
__ARGC:                                 | number of command line args
	.word   0
__ARGS:                                 | pointer to command line arg list
	.long   0
|
| uninitialized data space
|
.even
__base:                                 | pointer to basepage
   .long  0
_etext:                                 | pointer to end of text segment
   .long  0
_edata:                                 | pointer to end of data segment
   .long  0
_end:                                   | pointer to end of BSS (end of program)
   .long  0
__BREAK:                                | pointer to stack/heap break
   .long 0
__envp:                                 | pointer to environment string
   .long 0
traprtn:                                | storage for return PC in trap hooks
   .long 0
