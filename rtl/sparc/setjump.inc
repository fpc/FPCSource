{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Jonas Maebe and other members of the
    Free Pascal development team

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{#define ENV(base,reg) [%base + (reg * 4)]
#define ST_FLUSH_WINDOWS 3
#define RW_FP [%fp + 0x48]
}
procedure longjmp(var S:jmp_buf;value:longint);{assembler;}[Public,alias:'FPC_LONGJMP'];
  begin{asm
         /* Store our arguments in global registers so we can still
            use them while unwinding frames and their register windows.  */

         ld ENV(o0,JB_FP), %g3   /* Cache target FP in register %g3.  */
         mov %o0, %g1            /* ENV in %g1 */
         orcc %o1, %g0, %g2      /* VAL in %g2 */
         be,a 0f                 /* Branch if zero; else skip delay slot.  */
          mov 1, %g2             /* Delay slot only hit if zero: VAL = 1.  */
0:
         xor %fp, %g3, %o0
         add %fp, 512, %o1
         andncc %o0, 4095, %o0
         bne LOC(thread)
          cmp %o1, %g3
         bl LOC(thread)

         /* Now we will loop, unwinding the register windows up the stack
            until the restored %fp value matches the target value in %g3.  */

LOC(loop):
         cmp %fp, %g3            /* Have we reached the target frame? */
         bl,a LOC(loop)          /* Loop while current fp is below target.  */
          restore                /* Unwind register window in delay slot.  */
         be,a LOC(found)         /* Better have hit it exactly.  */
          ld ENV(g1,JB_SP), %o0  /* Delay slot: extract target SP.  */

LOC(thread):
         /*
          * Do a "flush register windows trap".  The trap handler in the
          * kernel writes all the register windows to their stack slots, and
          * marks them all as invalid (needing to be sucked up from the
          * stack when used).  This ensures that all information needed to
          * unwind to these callers is in memory, not in the register
          * windows.
          */
         ta      ST_FLUSH_WINDOWS
         ld      ENV(g1,JB_PC), %o7 /* Set return PC. */
         ld      ENV(g1,JB_SP), %fp /* Set saved SP on restore below. */
         sub     %fp, 64, %sp    /* Allocate a register frame. */
         st      %g3, RW_FP      /* Set saved FP on restore below. */
         retl
          restore %g2, 0, %o0    /* Restore values from above register frame. */

LOC(found):
         /* We have unwound register windows so %fp matches the target.  */
         mov %o0, %sp            /* OK, install new SP.  */

LOC(sp_ok):
         ld ENV(g1,JB_PC), %o0   /* Extract target return PC.  */
         jmp %o0 + 8             /* Return there.  */
          mov %g2, %o0           /* Delay slot: set return value.  */
}
end;
function setjmp(var S:jmp_buf):longint;{assembler;}[Public,alias:'FPC_SETJMP'];
  begin{asm
         b       1f
          set    0, %o1}
  end;
{ENTRY (__sigsetjmp)
1:
         /* Save our PC, SP and FP.  Save the signal mask if requested with
            a tail-call for simplicity; it always returns zero.  */
         ta      ST_FLUSH_WINDOWS

         st      %o7, [%o0 + (JB_PC * 4)]
         st      %sp, [%o0 + (JB_SP * 4)]
         st      %fp, [%o0 + (JB_FP * 4)]

         mov     %o7, %g1
         call    __sigjmp_save
          mov    %g1, %o7
END(__sigsetjmp)
/* Test if longjmp to JMPBUF would unwind the frame
    containing a local variable at ADDRESS.  */
#define _JMPBUF_UNWINDS(jmpbuf, address) \
   ((int) (address) < (jmpbuf)[JB_SP])
}
{
  $Log$
  Revision 1.4  2003-01-05 21:32:35  mazen
  * fixing several bugs compiling the RTL

  Revision 1.3  2002/12/24 21:30:20  mazen
  - some writeln(s) removed in compiler
  + many files added to RTL
  * some errors fixed in RTL

  Revision 1.2  2002/11/24 18:19:44  mazen
  + setjmp and longjmp

  Revision 1.1  2002/11/16 20:10:31  florian
    + sparc specific rtl skeleton added
}
