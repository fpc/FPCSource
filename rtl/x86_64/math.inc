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
      pushq $0
      fnstcw (%rsp)
      popq %rax
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
            subq $16,%rsp
            fnstcw -4(%rbp)
            fwait
            movw -4(%rbp),%cx
            orw $0x0c3f,%cx
            movw %cx,-8(%rbp)
            fldcw -8(%rbp)
            fwait
            fldt d
            frndint
            fldt d
            fsub %st(1),%st
            fstp %st(1)
            fnclex
            fldcw -4(%rbp)
      end;


    {$define FPC_SYSTEM_HAS_INT}
  {$ifdef INTERNCONSTINTF}
    function fpc_int_real(d : ValReal) : ValReal;assembler;compilerproc;
  {$else}
    function int(d : ValReal) : ValReal;assembler;[internconst:fpc_in_const_int];
  {$endif}
      asm
            subq $16,%rsp
            fnstcw -4(%rbp)
            fwait
            movw -4(%rbp),%cx
            orw $0x0c3f,%cx
            movw %cx,-8(%rbp)
            fldcw -8(%rbp)
            fwait
            fldt d
            frndint
            fnclex
            fldcw -4(%rbp)
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
            movq res,%rax
            fnclex
            fldcw oldcw
      end;


    {$define FPC_SYSTEM_HAS_ROUND}
{$ifdef internconstintf}
    function fpc_round_real(d : ValReal) : int64;assembler;compilerproc;
{$else}
  {$ifdef hascompilerproc}
    function round(d : extended) : int64;[internconst:fpc_in_const_round, external name 'FPC_ROUND'];

    function fpc_round(d : extended) : int64;assembler;[public, alias:'FPC_ROUND'];{$ifdef hascompilerproc}compilerproc;{$endif hascompilerproc}
  {$else}
    function round(d : extended) : int64;assembler;[internconst:fpc_in_const_round];
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
            fnclex
            fldcw newcw
            fwait
            fldt d
            fistpq res
            movq res,%rax
            fnclex
            fldcw oldcw
      end;


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
  Revision 1.8  2005-01-30 18:41:53  peter
    * add fnclex before loading old CW

  Revision 1.7  2005/01/30 18:01:15  peter
    * signal cleanup for linux
    * sigactionhandler instead of tsigaction for bsds
    * sigcontext moved to cpu dir

  Revision 1.6  2004/12/12 14:30:27  peter
    * x86_64 updates

  Revision 1.5  2004/12/12 12:41:46  peter
    * updated for latest rtl changes

  Revision 1.4  2004/04/24 18:31:25  florian
    * trunc and round for x86-64 fixed

  Revision 1.3  2004/02/06 15:58:21  florian
    * fixed x86-64 assembler problems

  Revision 1.2  2004/02/05 01:16:12  florian
    + completed x86-64/linux system unit

  Revision 1.1  2003/04/30 22:11:06  florian
    + for a lot of x86-64 dependend files mostly dummies added
}
