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
{$ifndef REGCALL}
      movw cw,%ax
{$endif}
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

{$ifdef INTERNCONSTINTF}
    {$define FPC_SYSTEM_HAS_PI}
    function fpc_pi_real : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$define FPC_SYSTEM_HAS_ABS}
    function fpc_abs_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$define FPC_SYSTEM_HAS_SQR}
    function fpc_sqr_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$define FPC_SYSTEM_HAS_SQRT}
    function fpc_sqrt_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$define FPC_SYSTEM_HAS_ARCTAN}
    function fpc_arctan_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$define FPC_SYSTEM_HAS_LN}
    function fpc_ln_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$define FPC_SYSTEM_HAS_SIN}
    function fpc_sin_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$define FPC_SYSTEM_HAS_COS}
    function fpc_cos_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
{$else}
    {$define FPC_SYSTEM_HAS_PI}
    function pi : ValReal;[internproc:fpc_in_pi];
    {$define FPC_SYSTEM_HAS_ABS}
    function abs(d : ValReal) : ValReal;[internproc:fpc_in_abs_real];
    {$define FPC_SYSTEM_HAS_SQR}
    function sqr(d : ValReal) : ValReal;[internproc:fpc_in_sqr_real];
    {$define FPC_SYSTEM_HAS_SQRT}
    function sqrt(d : ValReal) : ValReal;[internproc:fpc_in_sqrt_real];
    {$define FPC_SYSTEM_HAS_ARCTAN}
    function arctan(d : ValReal) : ValReal;[internproc:fpc_in_arctan_real];
    {$define FPC_SYSTEM_HAS_LN}
    function ln(d : ValReal) : ValReal;[internproc:fpc_in_ln_real];
    {$define FPC_SYSTEM_HAS_SIN}
    function sin(d : ValReal) : ValReal;[internproc:fpc_in_sin_real];
    {$define FPC_SYSTEM_HAS_COS}
    function cos(d : ValReal) : ValReal;[internproc:fpc_in_cos_real];
{$endif}

    {$define FPC_SYSTEM_HAS_EXP}
  {$ifdef INTERNCONSTINTF}
    function fpc_exp_real(d : ValReal) : ValReal;assembler;compilerproc;
  {$else}
    function exp(d : ValReal) : ValReal;assembler;[internconst:fpc_in_const_exp];
  {$endif}
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
  {$ifdef INTERNCONSTINTF}
    function fpc_frac_real(d : ValReal) : ValReal;assembler;compilerproc;
  {$else}
    function frac(d : ValReal) : ValReal;assembler;[internconst:fpc_in_const_frac];
  {$endif}
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
      end;


    {$define FPC_SYSTEM_HAS_INT}
  {$ifdef INTERNCONSTINTF}
    function fpc_int_real(d : ValReal) : ValReal;assembler;compilerproc;
  {$else}
    function int(d : ValReal) : ValReal;assembler;[internconst:fpc_in_const_int];
  {$endif}
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
      end;



    {$define FPC_SYSTEM_HAS_TRUNC}
  {$ifdef INTERNCONSTINTF}
    function fpc_trunc_real(d : ValReal) : int64;assembler;compilerproc;
  {$else}
    function trunc(d : ValReal) : int64;assembler;[internconst:fpc_in_const_trunc];
  {$endif}
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
            fclex
            fldcw oldcw
      end;


    {$define FPC_SYSTEM_HAS_ROUND}
  {$ifdef internconstintf}
    function fpc_round_real(d : ValReal) : int64;assembler;compilerproc;
  {$else}
    {$ifdef hascompilerproc}
      function round(d : ValReal) : int64;[internconst:fpc_in_const_round, external name 'FPC_ROUND'];
      function fpc_round(d : ValReal) : int64;assembler;[public, alias:'FPC_ROUND'];{$ifdef hascompilerproc}compilerproc;{$endif hascompilerproc}
    {$else}
      function round(d : ValReal) : int64;assembler;[internconst:fpc_in_const_round];
    {$endif hascompilerproc}
  {$endif}
      var
        oldcw,
        newcw : word;
        res   : int64;
      asm
            fnstcw oldcw
            fwait
            movw $0x1372,newcw
            fclex
            fldcw newcw
            fwait
            fldt d
            fistpq res
            movl res,%eax
            movl res+4,%edx
            fclex
            fldcw oldcw
      end;


    {$define FPC_SYSTEM_HAS_POWER}
   function power(bas,expo : ValReal) : ValReal;
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
        { bas < 0 is not allowed when doing roots }
         if (bas<0) and (frac(expo) <> 0) then
          handleerror(207)
         else
           begin
             power:=exp(ln(abs(bas))*expo);
             if (bas < 0) and
                odd(trunc(expo)) then
               begin
                 power := -power;
               end;
           end;
     end;

{
  $Log$
  Revision 1.21  2004-11-21 15:35:23  peter
    * float routines all use internproc and compilerproc helpers

  Revision 1.20  2004/11/17 22:19:04  peter
  internconst, internproc and some external declarations moved to interface

  Revision 1.19  2004/07/09 23:06:11  peter
    * add fclex for fpu exceptions to round/trunc

  Revision 1.18  2003/11/29 16:40:12  jonas
    * fix power() for negative base

  Revision 1.17  2003/11/24 21:57:43  michael
  + Patch from Johannes Berg for bug 2759

  Revision 1.16  2003/11/11 21:08:17  peter
    * REGCALL define added

  Revision 1.15  2003/09/08 18:21:37  peter
    * save edi,esi,ebx

  Revision 1.14  2003/04/23 21:28:21  peter
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
