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
                            FPU Control word
 ****************************************************************************}

    procedure Set8087CW(cw:word);assembler;
    asm
      movw cw,%ax
      movw %ax,default8087cw
      fnclex
      fldcw default8087cw
    end;

    function Get8087CW:word;assembler;
    asm
      pushl $0
      fnstcw (%esp)
      popl %eax
    end;

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
      var
        oldcw,
        newcw : word;
        res   : int64;
      asm
            fnstcw oldcw
            fwait
            movw oldcw,%cx
            orw $0x0c3f,%cx
            movw %cx,newcw
            fldcw newcw
            fwait
            fldt d
            fistpq res
            movl res,%eax
            movl res+4,%edx
            fldcw oldcw
      end ['EAX','ECX','EDX'];


    {$define FPC_SYSTEM_HAS_ROUND}
{$ifdef hascompilerproc}
    function round(d : extended) : int64;[internconst:in_const_round, external name 'FPC_ROUND'];
    
    function fpc_round(d : extended) : int64;assembler;[public, alias:'FPC_ROUND'];{$ifdef hascompilerproc}compilerproc;{$endif hascompilerproc}
{$else}
    function round(d : extended) : int64;assembler;[internconst:in_const_round];
{$endif hascompilerproc}
      var
        oldcw,
        newcw : word;
        res   : int64;
      asm
            fnstcw oldcw
            fwait
            movw $0x1372,newcw
            fldcw newcw
            fwait
            fldt d
            fistpq res
            movl res,%eax
            movl res+4,%edx
            fldcw oldcw
      end ['EAX','EDX'];


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

{
  $Log$
  Revision 1.14  2003-04-23 21:28:21  peter
    * fpc_round added, needed for int64 currency

  Revision 1.13  2003/02/05 19:53:17  carl
    * round bugfix with -Or switch

  Revision 1.12  2003/01/15 00:45:17  peter
    * use generic int64 power

  Revision 1.11  2003/01/15 00:40:18  peter
    * power returns int64

  Revision 1.10  2003/01/03 20:34:02  peter
    * i386 fpu controlword functions added

  Revision 1.9  2002/10/06 21:26:17  peter
    * round returns int64

  Revision 1.8  2002/09/07 16:01:19  peter
    * old logs removed and tabs fixed

}
