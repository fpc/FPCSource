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
      begin
        mxcsr:=w;
        asm
          ldmxcsr w
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
      asm
        fldt        d
        fldl2e
        fmul        %st(1),%st        { z = d * log2(e) }
        frndint
      { Calculate frac(z) using modular arithmetic to avoid precision loss.
        Avoid PIC hacks by using immediate operands (it's not the fastest,
        but likely the cleanest solution). }
        pushl       $0x3fe62e42       { ln(2).hi=6.9314718036912382E-001 }
        pushl       $0xfee00000
        fldl        (%esp)
        fmul        %st(1),%st
        fsubrp      %st,%st(2)
        pushl       $0x3dea39ef       { ln(2).lo=1.9082149292705877E-010 }
        pushl       $0x35793c76
        fldl        (%esp)
        fmul        %st(1),%st
        fsubrp      %st,%st(2)
        fxch        %st(1)            { (d-int(z)*ln2_hi)-int(z)*ln2_lo }
        fldl2e
        fmulp       %st,%st(1)        { frac(z) }

        fld         %st
        fabs
        fld1
        fcompp
        fstsw       %ax
        sahf
        jae         .L1               { frac(z) <= 1 }
        fld         %st(1)
        fabs
        pushl       $0x46c00000       { single(24576.0) }
        fcomps      (%esp)
        fstsw       %ax
        sahf
        jb          .L0               { int(z) < 24576 }
        fsub        %st,%st
        jmp         .L1
.L0:
        { Calculate 2**frac(z)-1 as N*(N+2), where N=2**(frac(z)/2)-1 }
        pushl       $0x3f000000       { single(0.5) }
        fmuls       (%esp)
        f2xm1
        fld         %st
        pushl       $0x40000000       { single(2.0) }
        fadds       (%esp)
        fmulp       %st,%st(1)
        jmp         .L2
.L1:
        f2xm1
.L2:
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
    


