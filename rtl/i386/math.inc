{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by the Free Pascal development team

    Implementation of mathematical routines (for extended type)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                       EXTENDED data type routines
 ****************************************************************************}

    {$define FPC_SYSTEM_HAS_PI}
    function pi : extended;[internproc:in_pi];
    {$define FPC_SYSTEM_HAS_ABS}
    function abs(d : extended) : extended;[internproc:in_abs_extended];
    {$define FPC_SYSTEM_HAS_SQR}
    function sqr(d : extended) : extended;[internproc:in_sqr_extended];
    {$define FPC_SYSTEM_HAS_SQRT}
    function sqrt(d : extended) : extended;[internproc:in_sqrt_extended];
    {$define FPC_SYSTEM_HAS_ARCTAN}
    function arctan(d : extended) : extended;[internproc:in_arctan_extended];
    {$define FPC_SYSTEM_HAS_LN}
    function ln(d : extended) : extended;[internproc:in_ln_extended];
    {$define FPC_SYSTEM_HAS_SIN}
    function sin(d : extended) : extended;[internproc:in_sin_extended];
    {$define FPC_SYSTEM_HAS_COS}
    function cos(d : extended) : extended;[internproc:in_cos_extended];

    {$define FPC_SYSTEM_HAS_EXP}
    function exp(d : extended) : extended;assembler;[internconst:in_const_exp];
       asm
            // comes from DJ GPP
            fldt        d
            fldl2e
            fmulp       %st,%st(1)
            fstcw      .LCW1
            fstcw      .LCW2
            andw        $0xf3ff,.LCW2
            orw         $0x0400,.LCW2
            fldcw      .LCW2
            fld         %st(0)
            frndint
            fldcw      .LCW1
            fxch        %st(1)
            fsub        %st(1),%st
            f2xm1
            fld1
            faddp       %st,%st(1)
            fscale
            fstp        %st(1)
            jmp         .LCW3
            // store some help data in the data segment
        .data
        .LCW1:
            .word       0
        .LCW2:
            .word       0
        .text
        .LCW3:
      end;


    {$define FPC_SYSTEM_HAS_FRAC}
    function frac(d : extended) : extended;assembler;[internconst:in_const_frac];
      asm
            subl $16,%esp
            fnstcw -4(%ebp)
            fwait
            movw -4(%ebp),%cx
            orw $0x0c3f,%cx
            movw %cx,-8(%ebp)
            fldcw -8(%ebp)
            fwait
            fldt d
            frndint
            fldt d
            fsub %st(1),%st
            fstp %st(1)
            fclex
            fldcw -4(%ebp)
      end ['ECX'];


    {$define FPC_SYSTEM_HAS_INT}
    function int(d : extended) : extended;assembler;[internconst:in_const_int];
      asm
            subl $16,%esp
            fnstcw -4(%ebp)
            fwait
            movw -4(%ebp),%cx
            orw $0x0c3f,%cx
            movw %cx,-8(%ebp)
            fldcw -8(%ebp)
            fwait
            fldt d
            frndint
            fclex
            fldcw -4(%ebp)
      end ['ECX'];



    {$define FPC_SYSTEM_HAS_TRUNC}
    function trunc(d : extended) : int64;assembler;[internconst:in_const_trunc];
      asm
            subl $16,%esp
            fnstcw -4(%ebp)
            fwait
            movw -4(%ebp),%cx
            orw $0x0c3f,%cx
            movw %cx,-8(%ebp)
            fldcw -8(%ebp)
            fwait
            fldt d
            fistpq -12(%ebp)
            movl -12(%ebp),%eax
            movl -8(%ebp),%edx
            fldcw -4(%ebp)
      end ['EAX','ECX','EDX'];


    {$define FPC_SYSTEM_HAS_ROUND}
    function round(d : extended) : longint;assembler;[internconst:in_const_round];
      asm
            subl $8,%esp
            fnstcw -4(%ebp)
            fwait
            movw $0x1372,-8(%ebp)
            fldcw -8(%ebp)
            fwait
            fldt d
            fistpl -8(%ebp)
            movl -8(%ebp),%eax
            fldcw -4(%ebp)
      end ['EAX','ECX'];


    {$define FPC_SYSTEM_HAS_POWER}
   function power(bas,expo : extended) : extended;
     begin
        if bas=0 then
          begin
            if expo<>0 then
              power:=0.0
            else
              HandleError(207);
          end
        else if expo=0 then
         power:=1
        else
        { bas < 0 is not allowed }
         if bas<0 then
          handleerror(207)
         else
          power:=exp(ln(bas)*expo);
     end;


{****************************************************************************
                       Longint data type routines
 ****************************************************************************}

   function power(bas,expo : longint) : longint;
     begin
        if bas=0 then
          begin
            if expo<>0 then
              power:=0
            else
              HandleError(207);
          end
        else if expo=0 then
         power:=1
        else
         begin
           if bas<0 then
            begin
              if odd(expo) then
               power:=-round(exp(ln(-bas)*expo))
              else
               power:=round(exp(ln(-bas)*expo));
            end
           else
            power:=round(exp(ln(bas)*expo));
         end;
     end;


{****************************************************************************
                    Helper routines to support old TP styled reals
 ****************************************************************************}

    function real2double(r : real48) : double;

      var
         res : array[0..7] of byte;
         exponent : word;

      begin
         { copy mantissa }
         res[0]:=0;
         res[1]:=r[1] shl 5;
         res[2]:=(r[1] shr 3) or (r[2] shl 5);
         res[3]:=(r[2] shr 3) or (r[3] shl 5);
         res[4]:=(r[3] shr 3) or (r[4] shl 5);
         res[5]:=(r[4] shr 3) or (r[5] and $7f) shl 5;
         res[6]:=(r[5] and $7f) shr 3;

         { copy exponent }
         { correct exponent: }
         exponent:=(word(r[0])+(1023-129));
         res[6]:=res[6] or ((exponent and $f) shl 4);
         res[7]:=exponent shr 4;

         { set sign }
         res[7]:=res[7] or (r[5] and $80);
         real2double:=double(res);
      end;

{
  $Log$
  Revision 1.6  2001-07-30 21:38:54  peter
    * m68k updates merged

  Revision 1.1.2.3  2001/07/29 23:56:28  carl
  - removed internamth define (always internal)

  Revision 1.1.2.2  2001/04/16 10:56:13  peter
    * fixes for stricter compiler

  Revision 1.1.2.1  2001/04/03 20:33:01  marco
   * Quickfixed trunc to int64 for Sebastian.

  Revision 1.1  2000/07/13 06:30:42  michael
  + Initial import
}
