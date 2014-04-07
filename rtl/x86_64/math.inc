{
    Implementation of mathematical routines for x86_64

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$push}
{$codealign constmin=16}
const
  FPC_ABSMASK_SINGLE: array[0..1] of qword=($7fffffff7fffffff,$7fffffff7fffffff); cvar; public;
  FPC_ABSMASK_DOUBLE: array[0..1] of qword=($7fffffffffffffff,$7fffffffffffffff); cvar; public;
{$pop}

{****************************************************************************
                            FPU Control word
 ****************************************************************************}

    procedure Set8087CW(cw:word);
      begin
         default8087cw:=cw;
         asm
           fnclex
           fldcw cw
         end;
      end;


    function Get8087CW:word;assembler;
      var
        tmp: word;
      asm
        fnstcw tmp
        movw   tmp,%ax
        andl   $0xffff,%eax  { clears bits 32-63 }
      end;


    procedure SetSSECSR(w : dword);
      begin
        mxcsr:=w;
        asm
          ldmxcsr w
        end;
      end;


    function GetSSECSR : dword;assembler;
      var
        _w : dword;
      asm
        stmxcsr _w
        movl    _w,%eax
      end;

{****************************************************************************
                       EXTENDED data type routines
 ****************************************************************************}

    {$ifndef FPC_SYSTEM_HAS_ABS}
    {$define FPC_SYSTEM_HAS_ABS}
    function fpc_abs_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$endif FPC_SYSTEM_HAS_ABS}
    {$ifndef FPC_SYSTEM_HAS_SQR}
    {$define FPC_SYSTEM_HAS_SQR}
    function fpc_sqr_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$endif FPC_SYSTEM_HAS_SQR}
    {$ifndef FPC_SYSTEM_HAS_SQRT}
    {$define FPC_SYSTEM_HAS_SQRT}
    function fpc_sqrt_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$endif FPC_SYSTEM_HAS_SQRT}

{$ifdef FPC_HAS_TYPE_EXTENDED}

    {$ifndef FPC_SYSTEM_HAS_ARCTAN}
    {$define FPC_SYSTEM_HAS_ARCTAN}
    function fpc_arctan_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$endif FPC_SYSTEM_HAS_ARCTAN}
    {$ifndef FPC_SYSTEM_HAS_LN}
    {$define FPC_SYSTEM_HAS_LN}
    function fpc_ln_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$endif FPC_SYSTEM_HAS_LN}
    {$ifndef FPC_SYSTEM_HAS_SIN}
    {$define FPC_SYSTEM_HAS_SIN}
    function fpc_sin_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$endif FPC_SYSTEM_HAS_SIN}
    {$ifndef FPC_SYSTEM_HAS_COS}
    {$define FPC_SYSTEM_HAS_COS}
    function fpc_cos_real(d : ValReal) : ValReal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$endif FPC_SYSTEM_HAS_COS}

    {$ifndef FPC_SYSTEM_HAS_EXP}
    {$define FPC_SYSTEM_HAS_EXP}
    function fpc_exp_real(d : ValReal) : ValReal;assembler;compilerproc;
       var
         oldcw,newcw: word;
       asm
            // comes from DJ GPP
            fldt        d
            fldl2e
            fmulp       %st,%st(1)
            fstcw       oldcw
            fstcw       newcw
            andw        $0xf3ff,newcw
            orw         $0x0400,newcw
            fldcw       newcw
            fld         %st(0)
            frndint
            fldcw       oldcw
            fxch        %st(1)
            fsub        %st(1),%st
            f2xm1
            fld1
            faddp       %st,%st(1)
            fscale
            fstp        %st(1)
      end;
    {$endif FPC_SYSTEM_HAS_EXP}


    {$ifndef FPC_SYSTEM_HAS_FRAC}
    {$define FPC_SYSTEM_HAS_FRAC}
    function fpc_frac_real(d : ValReal) : ValReal;assembler;compilerproc;
      var
        oldcw,newcw: word;
      asm
            fnstcw oldcw
            fwait
            movw oldcw,%cx
            orw $0x0c3f,%cx
            movw %cx,newcw
            fldcw newcw
            fwait
            fldt d
            frndint
            fldt d
            fsub %st(1),%st
            fstp %st(1)
            fnclex
            fldcw oldcw
      end;
    {$endif FPC_SYSTEM_HAS_FRAC}


    {$ifndef FPC_SYSTEM_HAS_INT}
    {$define FPC_SYSTEM_HAS_INT}
    function fpc_int_real(d : ValReal) : ValReal;assembler;compilerproc;
      var
        oldcw,newcw: word;
      asm
            fnstcw oldcw
            fwait
            movw oldcw,%cx
            orw $0x0c3f,%cx
            movw %cx,newcw
            fldcw newcw
            fwait
            fldt d
            frndint
            fwait
            fldcw oldcw
      end;
    {$endif FPC_SYSTEM_HAS_INT}


    {$ifndef FPC_SYSTEM_HAS_TRUNC}
    {$define FPC_SYSTEM_HAS_TRUNC}
    function fpc_trunc_real(d : ValReal) : int64;assembler;compilerproc;
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
        fldt d
        fistpq res
        fwait
        movq res,%rax
        fldcw oldcw
      end;
    {$endif FPC_SYSTEM_HAS_TRUNC}


    {$ifndef FPC_SYSTEM_HAS_ROUND}
    {$define FPC_SYSTEM_HAS_ROUND}
    function fpc_round_real(d : ValReal) : int64;assembler;compilerproc;
      var
        res   : int64;
      asm
        fldt d
        fistpq res
        fwait
        movq res,%rax
      end;
    {$endif FPC_SYSTEM_HAS_ROUND}

{$else FPC_HAS_TYPE_EXTENDED}

    {$define FPC_SYSTEM_HAS_TRUNC}
    function fpc_trunc_real(d : ValReal) : int64;compilerproc; assembler; nostackframe;
      asm
        cvttsd2si   %xmm0,%rax;
      end;

    {$define FPC_SYSTEM_HAS_ROUND}
    function fpc_round_real(d : ValReal) : int64;compilerproc; assembler; nostackframe;
      asm
        cvtsd2si   %xmm0,%rax;
      end;

{$endif FPC_HAS_TYPE_EXTENDED}
