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

    function pi : extended;[internproc:in_pi];
    function abs(d : extended) : extended;[internproc:in_abs_extended];
    function sqr(d : extended) : extended;[internproc:in_sqr_extended];
    function sqrt(d : extended) : extended;[internproc:in_sqrt_extended];
    function arctan(d : extended) : extended;[internproc:in_arctan_extended];
    function ln(d : extended) : extended;[internproc:in_ln_extended];
    function sin(d : extended) : extended;[internproc:in_sin_extended];
    function cos(d : extended) : extended;[internproc:in_cos_extended];

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

{****************************************************************************
                         Fixed data type routines
 ****************************************************************************}

{$ifdef HASFIXED} { Not yet allowed }

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

{$endif HASFIXED}

{
  $Log$
  Revision 1.5  2001-04-13 22:26:32  peter
    * remove warnings

  Revision 1.4  2000/10/21 18:20:17  florian
    * a lot of small changes:
       - setlength is internal
       - win32 graph unit extended
       ....

  Revision 1.3  2000/07/14 10:33:10  michael
  + Conditionals fixed

  Revision 1.2  2000/07/13 11:33:41  michael
  + removed logs

}