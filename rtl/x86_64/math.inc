
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
{-------------------------------------------------------------------------
 Using functions from AMath/DAMath libraries, which are covered by the
 following license:

 (C) Copyright 2009-2013 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------}

{$push}
{$codealign constmin=16}
const
  FPC_ABSMASK_SINGLE: array[0..1] of qword=($7fffffff7fffffff,$7fffffff7fffffff); cvar; public;
  FPC_ABSMASK_DOUBLE: array[0..1] of qword=($7fffffffffffffff,$7fffffffffffffff); cvar; public;
{$pop}
  FPC_LOG2E: Double   =  1.4426950408889634073599246810019; {  1/log(2)    }

  FPC_INFINITY_DOUBLE: QWord = $7ff0000000000000;     { IEEE 754 bit representation of +oo for binary64 }
  FPC_MAXLOG_DOUBLE: Double  =  709.78271289338399684324569237317; { log(2**1024)  }
  FPC_MINLOG_DOUBLE: Double  = -709.08956571282405153382846025171; { log(2**-1023) }

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


    procedure SetMXCSR(w : dword);
      begin
        defaultmxcsr:=w;
        asm
{$ifdef FPUX86_HAS_AVXUNIT}
          vldmxcsr w
{$else FPUX86_HAS_AVXUNIT}
          ldmxcsr w
{$endif FPUX86_HAS_AVXUNIT}
        end;
      end;


    function GetMXCSR : dword;assembler;
      var
        _w : dword;
      asm
{$ifdef FPUX86_HAS_AVXUNIT}
        vstmxcsr _w
{$else FPUX86_HAS_AVXUNIT}
        stmxcsr _w
{$endif FPUX86_HAS_AVXUNIT}
        movl    _w,%eax
      end;


      function GetNativeFPUControlWord: TNativeFPUControlWord; {$if defined(SYSTEMINLINE)}inline;{$endif}
        begin
          result.cw8087:=Get8087CW;
          result.MXCSR:=GetMXCSR
        end;


      procedure SetNativeFPUControlWord(const cw: TNativeFPUControlWord); {$if defined(SYSTEMINLINE)}inline;{$endif}
        begin
          Set8087CW(cw.cw8087);
          SetMXCSR(cw.MXCSR);
        end;


      procedure SetSSECSR(w : dword);
        begin
          SetMXCSR(w);
        end;


    function GetSSECSR: dword;
      begin
        result:=GetMXCSR;
      end;

{****************************************************************************
                       EXTENDED data type routines
 ****************************************************************************}

    {$ifndef FPC_SYSTEM_HAS_ABS}
    {$define FPC_SYSTEM_HAS_ABS}
    function fpc_abs_real(d : ValReal) : ValReal;compilerproc;
{$ifndef cpullvm}
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
{$else not cpullvm}
    assembler;
    asm
      fldt d
      fabs
{$endif not cpullvm}
    end;
    {$endif FPC_SYSTEM_HAS_ABS}
    {$ifndef FPC_SYSTEM_HAS_SQR}
    {$define FPC_SYSTEM_HAS_SQR}
    function fpc_sqr_real(d : ValReal) : ValReal;compilerproc;
{$ifndef cpullvm}
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
{$else not cpullvm}
    begin
      fpc_sqr_real:=d*d;
{$endif not cpullvm}
    end;
    {$endif FPC_SYSTEM_HAS_SQR}
    {$ifndef FPC_SYSTEM_HAS_SQRT}
    {$define FPC_SYSTEM_HAS_SQRT}
    function fpc_sqrt_real(d : ValReal) : ValReal;compilerproc;
{$ifndef cpullvm}
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
{$else not cpullvm}
    assembler;
    asm
      fldt d
      fsqrt
{$endif not cpullvm}
    end;
    {$endif FPC_SYSTEM_HAS_SQRT}

{$ifdef FPC_HAS_TYPE_EXTENDED}

    {$ifndef FPC_SYSTEM_HAS_ARCTAN}
    {$define FPC_SYSTEM_HAS_ARCTAN}
    function fpc_arctan_real(d : ValReal) : ValReal;compilerproc;
{$ifndef cpullvm}
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
{$else not cpullvm}
    assembler;
    asm
      fldt d
      fld1
      fpatan
{$endif not cpullvm}
    end;
    {$endif FPC_SYSTEM_HAS_ARCTAN}
    {$ifndef FPC_SYSTEM_HAS_LN}
    {$define FPC_SYSTEM_HAS_LN}
    function fpc_ln_real(d : ValReal) : ValReal;compilerproc;
{$ifndef cpullvm}
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
{$else not cpullvm}
    assembler;
    asm
      fldln2
      fldt d
      fyl2x
{$endif not cpullvm}
    end;
    {$endif FPC_SYSTEM_HAS_LN}
    {$ifndef FPC_SYSTEM_HAS_SIN}
    {$define FPC_SYSTEM_HAS_SIN}
    function fpc_sin_real(d : ValReal) : ValReal;compilerproc;
{$ifndef cpullvm}
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
{$else not cpullvm}
    assembler;
    asm
      fldt d
      fsin
{$endif not cpullvm}
    end;
    {$endif FPC_SYSTEM_HAS_SIN}
    {$ifndef FPC_SYSTEM_HAS_COS}
    {$define FPC_SYSTEM_HAS_COS}
    function fpc_cos_real(d : ValReal) : ValReal;compilerproc;
{$ifndef cpullvm}
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
{$else not cpullvm}
    assembler;
    asm
      fldt d
      fcos
{$endif not cpullvm}
    end;
    {$endif FPC_SYSTEM_HAS_COS}

    {$ifndef FPC_SYSTEM_HAS_EXP}
    {$define FPC_SYSTEM_HAS_EXP}
    { exp function adapted from AMath library (C) Copyright 2009-2013 Wolfgang Ehrhardt
      * translated into AT&T syntax
      + PIC support
      * return +Inf/0 for +Inf/-Inf input, instead of NaN }
    function fpc_exp_real(d : ValReal) : ValReal;assembler;compilerproc;
      const
        ln2hi: double=6.9314718036912382E-001;
        ln2lo: double=1.9082149292705877E-010;
        large: single=24576.0;
        two:   single=2.0;
        half:  single=0.5;
      asm
            fldt        d
            fldl2e
            fmul        %st(1),%st        { z = d * log2(e) }
            frndint
          { Calculate frac(z) using modular arithmetic to avoid precision loss }
            fldl        ln2hi(%rip)
            fmul        %st(1),%st
            fsubrp      %st,%st(2)
            fldl        ln2lo(%rip)
            fmul        %st(1),%st
            fsubrp      %st,%st(2)
            fxch        %st(1)            { (d-int(z)*ln2_hi)-int(z)*ln2_lo }
            fldl2e
            fmulp       %st,%st(1)        { frac(z) }

          { Above calculation can yield |frac(z)|>1, particularly when rounding mode
            is not "round to nearest". f2xm1 is undefined in that case, so it's
            necessary to check }
            fld         %st
            fabs
            fld1
            fcomip      %st(1),%st(0)
            fstp        %st
            jp          .L3               { NaN }
            jae         .L1               { |frac(z)| <= 1, good }

            fld         %st(1)
            fabs
            flds        large(%rip)
            fcomip      %st(1),%st(0)
            fstp        %st
            jb          .L3               { int(z) >= 24576 }
   .L0:
            { Calculate 2**frac(z)-1 as N*(N+2), where N=2**(frac(z)/2)-1 }
            fmuls       half(%rip)
            f2xm1
            fld         %st
            fadds       two(%rip)
            fmulp       %st,%st(1)
            jmp         .L2
   .L3:
            fstp        %st               { pop frac(z) and load 0 }
            fldz
   .L1:
            f2xm1
   .L2:
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
            fldt d
            movw oldcw,%cx
            orw $0x0c00,%cx
            movw %cx,newcw
            fldcw newcw
            fld %st
            frndint
            fsubrp %st,%st(1)
            fwait
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
            movw oldcw,%cx
            orw $0x0c00,%cx
            movw %cx,newcw
            fldcw newcw
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
        movw oldcw,%cx
        orw $0x0c00,%cx
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

    {$ifndef FPC_SYSTEM_HAS_INT}
    {$define FPC_SYSTEM_HAS_INT}
    function fpc_int_real(d : ValReal) : ValReal;compilerproc; assembler; nostackframe;
      asm
        pextrw    $3, %xmm0, %eax { eax = d[48:63] }
        and       $0x7ff0,%ax
        cmp       $0x4330,%ax
        jge       .L0
        cvttsd2si %xmm0,  %rax
        cvtsi2sd  %rax,   %xmm0
    .L0:
      end;
    {$endif FPC_SYSTEM_HAS_INT}

    {$ifndef FPC_SYSTEM_HAS_TRUNC}
    {$define FPC_SYSTEM_HAS_TRUNC}
    function fpc_trunc_real(d : ValReal) : int64;compilerproc; assembler; nostackframe;
      asm
        cvttsd2si   %xmm0,%rax;
      end;
    {$endif FPC_SYSTEM_HAS_TRUNC}

    {$ifndef FPC_SYSTEM_HAS_ROUND}
    {$define FPC_SYSTEM_HAS_ROUND}
    function fpc_round_real(d : ValReal) : int64;compilerproc; assembler; nostackframe;
      asm
        cvtsd2si   %xmm0,%rax;
      end;
    {$endif FPC_SYSTEM_HAS_ROUND}

    {$ifndef FPC_SYSTEM_HAS_FRAC}
    {$define FPC_SYSTEM_HAS_FRAC}
    function fpc_frac_real(d: ValReal) : ValReal;compilerproc; assembler; nostackframe;
      asm
        { Windows defines %xmm4 and %xmm5 as first non-parameter volatile registers;
          on SYSV systems all are considered as such, so use %xmm4 }
        pextrw    $3, %xmm0, %eax { eax = d[48:63] }
        movapd    %xmm0,  %xmm4
        and       $0x7ff0,%ax
        cmp       $0x4330,%ax
        jge       .L0
        cvttsd2si %xmm0,  %rax
        cvtsi2sd  %rax,   %xmm4
  .L0:
        subsd     %xmm4,  %xmm0
      end;
    {$endif FPC_SYSTEM_HAS_FRAC}

    {$ifndef FPC_SYSTEM_HAS_EXP}
    {$define FPC_SYSTEM_HAS_EXP}

{$push}
{$codealign constmin=16}
  const
    { coefficients of P(-x) and P(x), P(x) = x^6 + 42x^5 + 840x^4 + 10080x^3 + 75600x^2 + 332640x + 665280 }
    FPC_EXP_C5_ARRAY: array[0..1] of Double = (-42.0,     42.0);
    FPC_EXP_C4_ARRAY: array[0..1] of Double = (840.0,     840.0);
    FPC_EXP_C3_ARRAY: array[0..1] of Double = (-10080.0,  10080.0);
    FPC_EXP_C2_ARRAY: array[0..1] of Double = (75600.0,   75600.0);
    FPC_EXP_C1_ARRAY: array[0..1] of Double = (-332640.0, 332640.0);
    FPC_EXP_C0_ARRAY: array[0..1] of Double = (665280.0,  665280.0);
{$pop}
    FPC_HALF_DOUBLE: Double = 0.5;
    FPC_ONE_DOUBLE: Double = 1.0;
    { ln 2 = FPC_LN2_HI + FPC_LN2_LO; k * FPC_LN2_HI is exact for abs(k) <= 1024 }
    FPC_LN2_HI: QWord = $3FE62E42FEE00000;
    FPC_LN2_LO: QWord = $3DEA39EF35793C76;

    function fpc_exp_real(d : ValReal) : ValReal; compilerproc; assembler; nostackframe;
    {*****************************************************************}
    { Exponential Function                                            }
    {*****************************************************************}
    {                                                                 }
    { SYNOPSIS:                                                       }
    {                                                                 }
    { double d, y, exp();                                             }
    {                                                                 }
    { y = exp(d);                                                     }
    {                                                                 }
    { DESCRIPTION:                                                    }
    {                                                                 }
    { Returns e (2.71828...) raised to the d power.                   }
    {                                                                 }
    { First calculate z = d log2 e, then break z into integer and     }
    { fractional components k and f.                                  }
    {                                                                 }
    {     d    z      k + f    k f                                    }
    {    e  = 2   => 2      = 2 2                                     }
    {                                                                 }
    { 2^k can be calculated very quickly with bit manipulation, then  }
    { x = d - k ln 2 = f ln 2 and a Padé approximant gives e^x for x  }
    { between -0.5 ln 2 and 0.5 ln 2                                  }
    {*****************************************************************}
      asm
        movsd     FPC_LOG2E(%rip), %xmm4
        ucomisd   FPC_MAXLOG_DOUBLE(%rip), %xmm0
        jp        .LNaN { Propagate NaN into result }
        jae       .LOverflow
        movsd     FPC_ONE_DOUBLE(%rip), %xmm5
        xorpd     %xmm3, %xmm3
        comisd    FPC_MINLOG_DOUBLE(%rip), %xmm0
        ja        .LNoUnderflow
        xorpd     %xmm0, %xmm0 { Set result to zero }
    .LNaN:
        ret
    .LOverflow:
        movsd     FPC_INFINITY_DOUBLE(%rip), %xmm0 { Set result to infinity }
        ret
        .balign   16
    .LNoUnderflow:
        movsd     FPC_HALF_DOUBLE(%rip), %xmm2
        comisd    %xmm3, %xmm0
        jnz .LNotZero
        movsd     %xmm5, %xmm0 { e^0 = 1 }
        ret
        .balign   16
    .LNotZero:
        movapd    %xmm0, %xmm5 { d }
        mulsd     %xmm4, %xmm0 { z }
        { k = trunc(z + copysign(0.5, z)): z rounded half away from zero }
        movupd    FPC_ABSMASK_DOUBLE(%rip), %xmm1
        andnpd    %xmm0, %xmm1 { sign of z }
        orpd      %xmm2, %xmm1 { copysign(0.5, z) }
        addsd     %xmm0, %xmm1
        { %xmm1 won't be out of range due to the earlier checks }
        cvttsd2si %xmm1, %rax
        cvtsi2sd  %rax,  %xmm1 { k }

        { x = (d - k ln2_hi) - k ln2_lo }
        movsd     FPC_LN2_HI(%rip), %xmm0
        mulsd     %xmm1, %xmm0
        subsd     %xmm0, %xmm5
        mulsd     FPC_LN2_LO(%rip), %xmm1
        subsd     %xmm1, %xmm5
        movapd    %xmm5, %xmm0 { x }

        { %rcx: bits of the extra factor 2.0 for k=1024 or 0.5 for k=-1023, 0 otherwise }
        xorl      %ecx,  %ecx
        cmpl      $1023, %eax
        jg        .LBigK
        cmpl      $-1022, %eax
        jl        .LSmallK
    .LKReady:
        { Some slightly evil floating-point bit manipulation to calculate 2^k }
        addw      $1023, %ax
        shlw      $4,    %ax
        pinsrw $3,%eax,  %xmm3 { Insert into the most-significant 16 bits }
                               { %xmm3 now contains 2^k }

        { [6/6] Padé approximant e^x = P(x) / P(-x), both evaluated with Horner, P(-x) in the low lane }
        shufpd $0,%xmm0, %xmm0 { x, x }
        movapd    %xmm0, %xmm4 { x, x }
        addpd     FPC_EXP_C5_ARRAY(%rip), %xmm0
        mulpd     %xmm4, %xmm0
        addpd     FPC_EXP_C4_ARRAY(%rip), %xmm0
        mulpd     %xmm4, %xmm0
        addpd     FPC_EXP_C3_ARRAY(%rip), %xmm0
        mulpd     %xmm4, %xmm0
        addpd     FPC_EXP_C2_ARRAY(%rip), %xmm0
        mulpd     %xmm4, %xmm0
        addpd     FPC_EXP_C1_ARRAY(%rip), %xmm0
        mulpd     %xmm4, %xmm0
        addpd     FPC_EXP_C0_ARRAY(%rip), %xmm0 { P(-x), P(x) }

        movapd    %xmm0, %xmm4
        shufpd $1,%xmm0, %xmm0 { P(x), P(-x) }
        divsd     %xmm4, %xmm0 { e^x }

        mulsd     %xmm3, %xmm0 { Apply the integer component calculated earlier }
        testq     %rcx,  %rcx
        jnz       .LExtraFactor
        ret
    .LExtraFactor:
        movq      %rcx,  %xmm1
        mulsd     %xmm1, %xmm0
        ret
    .LBigK:
        decl      %eax { 2^1024 = 2^1023 * 2.0 }
        movl      $0x400, %ecx
        shlq      $52,   %rcx
        jmp       .LKReady
    .LSmallK:
        incl      %eax { 2^-1023 = 2^-1022 * 0.5 }
        movl      $0x3FE, %ecx
        shlq      $52,   %rcx
        jmp       .LKReady
      end;
    {$endif FPC_SYSTEM_HAS_EXP}

{$endif FPC_HAS_TYPE_EXTENDED}
