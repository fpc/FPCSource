{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This file contains some helper routines for int64 and qword

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define FPC_SYSTEM_HAS_DIV_QWORD}
    function fpc_div_qword(n,z : qword) : qword;assembler;[public,alias: 'FPC_DIV_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
      { from the ppc compiler writers guide }
      assembler;
      asm
        // (R5:R6) = (R5:R6) / (R3:R4) (64b) = (64b / 64b)
        // quo        dvd         dvs
        //
        // Remainder is returned in R3:R4.
        //
        // Code comment notation:
        // msw = most-significant (high-order) word, i.e. bits 0..31
        // lsw = least-significant (low-order) word, i.e. bits 32..63
        // LZ = Leading Zeroes
        // SD = Significant Digits
        //
        // R5:R6 = dvd (input dividend); quo (output quotient)
        // R3:R4 = dvs (input divisor); rem (output remainder)
        //
        // R7:R8 = tmp
        // count the number of leading 0s in the dividend
        or.     R0,R3,R4 // dvs = 0?
        cmpwi   cr1,R5,0 // dvd.msw == 0?
        cntlzw  R0,R5 // R0 = dvd.msw.LZ
        cntlzw  R9,R6 // R9 = dvd.lsw.LZ
        bne+    .LNoDivByZero
        b       FPC_DIVBYZERO
      .LNoDivByZero:
        bne     cr1,.Llab1 // if(dvd.msw == 0) dvd.LZ = dvd.msw.LZ
        addi    R0,R9,32 // dvd.LZ = dvd.lsw.LZ + 32
      .Llab1:
        // count the number of leading 0s in the divisor
        cmpwi   cr0,R3,0 // dvd.msw == 0?
        cntlzw  R9,R3 // R9 = dvs.msw.LZ
        cntlzw  R10,R4 // R10 = dvs.lsw.LZ
        bne     cr0,.Llab2 // if(dvs.msw == 0) dvs.LZ = dvs.msw.LZ
        addi    R9,R10,32 // dvs.LZ = dvs.lsw.LZ + 32
      .Llab2:
        // determine shift amounts to minimize the number of iterations
        cmpw    cr0,R0,R9 // compare dvd.LZ to dvs.LZ
        subfic  R10,R0,64 // R10 = dvd.SD
        bgt     cr0,.Llab9 // if(dvs > dvd) quotient = 0
        addi    R9,R9,1 // ++dvs.LZ (or --dvs.SD)
        subfic  R9,R9,64 // R9 = dvs.SD
        add     R0,R0,R9 // (dvd.LZ + dvs.SD) = left shift of dvd for
        // initial dvd
        subf    R9,R9,R10 // (dvd.SD - dvs.SD) = right shift of dvd for
        // initial tmp
        mtctr   R9 // number of iterations = dvd.SD - dvs.SD
        // R7:R8 = R5:R6 >> R9
        cmpwi   cr0,R9,32 // compare R9 to 32
        addi    R7,R9,-32
        blt     cr0,.Llab3 // if(R9 < 32) jump to .Llab3
        srw     R8,R5,R7 // tmp.lsw = dvd.msw >> (R9 - 32)
        li      R7,0 // tmp.msw = 0
        b       .Llab4
      .Llab3:
        srw     R8,R6,R9 // R8 = dvd.lsw >> R9
        subfic  R7,R9,32
        slw     R7,R5,R7 // R7 = dvd.msw << 32 - R9
        or      R8,R8,R7 // tmp.lsw = R8 | R7
        srw     R7,R5,R9 // tmp.msw = dvd.msw >> R9
      .Llab4:
        // R5:R6 = R5:R6 << R0
        cmpwi   cr0,R0,32 // compare R0 to 32
        addic   R9,R0,-32
        blt     cr0,.Llab5 // if(R0 < 32) jump to .Llab5
        slw     R5,R6,R9 // dvd.msw = dvd.lsw << R9
        li      R6,0 // dvd.lsw = 0
        b       .Llab6
      .Llab5:
        slw     R5,R5,R0 // R5 = dvd.msw << R0
        subfic  R9,R0,32
        srw     R9,R6,R9 // R9 = dvd.lsw >> 32 - R0
        or      R5,R5,R9 // dvd.msw = R5 | R9
        slw     R6,R6,R0 // dvd.lsw = dvd.lsw << R0
      .Llab6:
        // restoring division shift and subtract loop
        li      R10,-1 // R10 = -1
        addic   R7,R7,0 // clear carry bit before loop starts
      .Llab7:
        // tmp:dvd is considered one large register
        // each portion is shifted left 1 bit by adding it to itself
        // adde sums the carry from the previous and creates a new carry
        adde    R6,R6,R6 // shift dvd.lsw left 1 bit
        adde    R5,R5,R5 // shift dvd.msw to left 1 bit
        adde    R8,R8,R8 // shift tmp.lsw to left 1 bit
        adde    R7,R7,R7 // shift tmp.msw to left 1 bit
        subfc   R0,R4,R8 // tmp.lsw - dvs.lsw
        subfe.  R9,R3,R7 // tmp.msw - dvs.msw
        blt     cr0,.Llab8 // if(result < 0) clear carry bit
        mr      R8,R0 // move lsw
        mr      R7,R9 // move msw
        addic   R0,R10,1 // set carry bit
      .Llab8:
        bdnz    .Llab7
        // write quotient and remainder
        adde    R4,R6,R6 // quo.lsw (lsb = CA)
        adde    R3,R5,R5 // quo.msw (lsb from lsw)
        mr      R6,R8 // rem.lsw
        mr      R5,R7 // rem.msw
        b       .Lqworddivdone // return
      .Llab9:
        // Quotient is 0 (dvs > dvd)
        li     R4,0 // dvd.lsw = 0
        li     R3,0 // dvd.msw = 0
      .Lqworddivdone:
      end;


{$define FPC_SYSTEM_HAS_MOD_QWORD}
    function fpc_mod_qword(n,z : qword) : qword;assembler;[public,alias: 'FPC_MOD_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
      assembler;
      var
        oldlr: pointer;
      asm
        mflr r0
        stw  r0,oldlr
        bl   FPC_DIV_QWORD
        lwz  r0,oldlr
        mtlr r0
        mr   R3,R5
        mr   R4,R6
      end;

{
  $Log$
  Revision 1.2  2004-01-12 18:03:30  jonas
    + ppc implemetnation of fpc_mod/div_qword (from ppc compiler writers guide)

  Revision 1.1  2003/09/14 11:34:13  peter
    * moved int64 asm code to int64p.inc
    * save ebx,esi

}
