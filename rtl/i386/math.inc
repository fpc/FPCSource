{
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

    procedure Set8087CW(cw:word);
      begin
        { pic-safe ; cw will not be a regvar because it's accessed from }
        { assembler                                                     }
        default8087cw:=cw;
        asm
          fnclex
          fldcw cw
        end;
      end;


    function Get8087CW:word;assembler;
      asm
        pushl $0
        fnstcw (%esp)
        popl %eax
      end;


    procedure SetSSECSR(w : dword);
      var
        _w : dword;
      begin
        _w:=w;
        asm
          ldmxcsr _w
        end;
      end;


    function GetSSECSR : dword;
      var
        _w : dword;
      begin
        asm
          stmxcsr _w
        end;
        result:=_w;
      end;

{****************************************************************************
                       EXTENDED data type routines
 ****************************************************************************}

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

  {$define FPC_SYSTEM_HAS_EXP}
    function fpc_exp_real(d : ValReal) : ValReal;assembler;compilerproc;
      var
        cw1,cw2: word;
      asm
        // comes from DJ GPP
        fldt        d
        fldl2e
        fmulp       %st,%st(1)
        fstcw       CW1
        fstcw       CW2
        fwait
        andw        $0xf3ff,CW2
        orw         $0x0400,CW2
        fldcw       CW2
        fld         %st(0)
        frndint
        fldcw       CW1
        fxch        %st(1)
        fsub        %st(1),%st
        f2xm1
        fld1
        faddp       %st,%st(1)
        fscale
        fstp        %st(1)
     end;


    {$define FPC_SYSTEM_HAS_FRAC}
    function fpc_frac_real(d : ValReal) : ValReal;assembler;compilerproc;
      asm
        subl $4,%esp
        fnstcw (%esp)
        fwait
        movw (%esp),%cx
        orw $0x0f00,(%esp)
        fldcw (%esp)
        fldt d
        frndint
        fldt d
        fsub %st(1),%st
        fstp %st(1)
        movw %cx,(%esp)
        fldcw (%esp)
      end;


    {$define FPC_SYSTEM_HAS_INT}
    function fpc_int_real(d : ValReal) : ValReal;assembler;compilerproc;
      asm
        subl $4,%esp
        fnstcw (%esp)
        fwait
        movw (%esp),%cx
        orw $0x0f00,(%esp)
        fldcw (%esp)
        fwait
        fldt d
        frndint
        fwait
        movw %cx,(%esp)
        fldcw (%esp)
      end;


    {$define FPC_SYSTEM_HAS_TRUNC}
    function fpc_trunc_real(d : ValReal) : int64;assembler;compilerproc;
      asm
        subl $12,%esp
        fldt d
        fnstcw (%esp)
        movw (%esp),%cx
        orw $0x0f00,(%esp)
        fldcw (%esp)
        movw %cx,(%esp)
        fistpq 4(%esp)
        fldcw (%esp)
        fwait
        movl 4(%esp),%eax
        movl 8(%esp),%edx
      end;


    {$define FPC_SYSTEM_HAS_ROUND}
    { keep for bootstrapping with 2.0.x }
    function fpc_round_real(d : ValReal) : int64;compilerproc;assembler;
      var
        res   : int64;
      asm
        fldt d
        fistpq res
        fwait
        movl res,%eax
        movl res+4,%edx
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

