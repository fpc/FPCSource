{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implementation of mathamatical Routines (only for real)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                       EXTENDED data type routines
 ****************************************************************************}

{$ifdef hasinternmath}
    function pi : extended;[internproc:in_pi];
    function abs(d : extended) : extended;[internproc:in_abs_extended];
    function sqr(d : extended) : extended;[internproc:in_sqr_extended];
    function sqrt(d : extended) : extended;[internproc:in_sqrt_extended];
    function arctan(d : extended) : extended;[internproc:in_arctan_extended];
    function ln(d : extended) : extended;[internproc:in_ln_extended];
    function sin(d : extended) : extended;[internproc:in_sin_extended];
    function cos(d : extended) : extended;[internproc:in_cos_extended];
{$else hasinternmath}
    function pi : extended;assembler;[internconst:in_const_pi];
      asm
            fldpi
      end [];


    function abs(d : extended) : extended;assembler;[internconst:in_const_abs];
      asm
            fldt d
            fabs
      end [];


    function sqr(d : extended) : extended;assembler;[internconst:in_const_sqr];
      asm
            fldt d
            fldt d
            fmulp
      end [];


    function sqrt(d : extended) : extended;assembler;[internconst:in_const_sqrt];
      asm
            fldt d
            fsqrt
      end [];


    function arctan(d : extended) : extended;assembler;[internconst:in_const_arctan];
      asm
            fldt d
            fld1
            fpatan
      end [];

    function cos(d : extended) : extended;assembler;[internconst:in_const_cos];
      asm
            fldt d
            fcos
            fstsw
            sahf
            jnp .LCOS1
            fstp %st(0)
            fldt .LCOS0
            jmp .LCOS1
         .data
         .LCOS0:
            .long       0xffffffff
            .long       0xffffffff
            .long       0xffffffff
         .text
         .LCOS1:
      end ['EAX'];

    function ln(d : extended) : extended;assembler;[internconst:in_const_ln];
      asm
            fldln2
            fldt d
            fyl2x
      end [];


    function sin(d : extended) : extended;assembler;[internconst:in_const_sin];
      asm
            fldt d
            fsin
            fstsw
            sahf
            jnp .LSIN1
            fstp %st(0)
            fldt .LSIN0
            jmp .LSIN1
         .data
         .LSIN0:
            .long       0xffffffff
            .long       0xffffffff
            .long       0xffffffff
         .text
         .LSIN1:
      end ['EAX'];


{$endif hasinternmath}

    function exp(d : extended) : extended;assembler;[internconst:in_const_exp];
       asm
            // comes from DJ GPP
            fldt        d
            fldl2e
            fmulp
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
            faddp
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
            fsub %st(1)
            fstp %st(1)
            fclex
            fldcw -4(%ebp)
      end ['ECX'];


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


    function trunc(d : extended) : longint;assembler;[internconst:in_const_trunc];
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
            fistpl -8(%ebp)
            movl -8(%ebp),%eax
            fldcw -4(%ebp)
      end ['EAX','ECX'];


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


   function power(bas,expo : extended) : extended;
     begin
        if expo=0 then
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
        if expo=0 then
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
                         Fixed data type routines
 ****************************************************************************}

{$ifdef _SUPPORT_FIXED} { Not yet allowed }

    function sqrt(d : fixed) : fixed;

      begin
         asm
            movl d,%eax
            movl %eax,%ebx
            movl %eax,%ecx
            jecxz .L_kl
            xorl %esi,%esi
         .L_it:
            xorl %edx,%edx
            idivl %ebx
            addl %ebx,%eax
            shrl $1,%eax
            subl %eax,%esi
            cmpl $1,%esi
            jbe .L_kl
            movl %eax,%esi
            movl %eax,%ebx
            movl %ecx,%eax
            jmp .L_it
         .L_kl:
            shl $8,%eax
            leave
            ret $4
         end;
      end;


    function int(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the integral part of d                                  }
    {*****************************************************************}
    begin
      int:=d and $ffff0000;       { keep only upper bits      }
    end;


    function trunc(d : fixed) : longint;
    {*****************************************************************}
    { Returns the Truncated integral part of d                        }
    {*****************************************************************}
    begin
      trunc:=longint(integer(d shr 16));   { keep only upper 16 bits  }
    end;

    function frac(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the Fractional part of d                                }
    {*****************************************************************}
    begin
      frac:=d AND $ffff;         { keep only decimal parts - lower 16 bits }
    end;

    function abs(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the Absolute value of d                                 }
    {*****************************************************************}
    begin
       asm
           movl d,%eax
           rol $16,%eax             { Swap high & low word.}
           {Absolute value: Invert all bits and increment when <0 .}
           cwd                      { When ax<0, dx contains $ffff}
           xorw %dx,%ax             { Inverts all bits when dx=$ffff.}
           subw %dx,%ax             { Increments when dx=$ffff.}
           rol $16,%eax             { Swap high & low word.}
           leave
           ret $4
       end;
    end;


    function sqr(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the Absolute squared value of d                         }
    {*****************************************************************}
    begin
            {16-bit precision needed, not 32 =)}
       sqr := d*d;
{       sqr := (d SHR 8 * d) SHR 8; }
    end;


    function Round(x: fixed): longint;
    {*****************************************************************}
    { Returns the Rounded value of d as a longint                     }
    {*****************************************************************}
    var
     lowf:integer;
     highf:integer;
    begin
      lowf:=x and $ffff;       { keep decimal part ... }
      highf :=integer(x shr 16);
      if lowf > 5 then
        highf:=highf+1
      else
      if lowf = 5 then
      begin
        { here we must check the sign ...       }
        { if greater or equal to zero, then     }
        { greater value will be found by adding }
        { one...                                }
         if highf >= 0 then
           Highf:=Highf+1;
      end;
      Round:= longint(highf);
    end;

{$endif SUPPORT_FIXED}

{
  $Log$
  Revision 1.20  2000-02-09 16:59:29  peter
    * truncated log

  Revision 1.19  2000/01/07 16:41:33  daniel
    * copyright 2000

  Revision 1.18  2000/01/07 16:32:24  daniel
    * copyright 2000 added

  Revision 1.17  1999/10/06 17:44:43  peter
    * fixed power(int,int) with negative base
    * power(ext,ext) with negative base gives rte 207

  Revision 1.16  1999/09/15 20:24:11  florian
    * some math functions are now coded inline by the compiler

}
