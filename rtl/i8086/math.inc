{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    Implementation of mathematical Routines (for extended type)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode intel}

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
        push bp
        mov bp, sp
        push ax
        fnstcw [bp - 2]
        pop ax
        mov sp, bp
        pop bp
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

    {$define FPC_SYSTEM_HAS_EXP}
    function fpc_exp_real(d : ValReal) : ValReal;assembler;compilerproc;
      var
        sw1: word;
      asm
        // comes from DJ GPP
        { fixed for 8087 and 80287 by nickysn
          notable differences between 8087/80287 and 80387:
            f2xm1  on 8087/80287  requires that  0<=st(0)<=0.5
            f2xm1  on      80387+ requires that -1<=st(0)<=1
            fscale on 8087/80287  requires that -2**15<=st(1)<=0 or 1<=st(1)<2**15
            fscale on      80387+ has no restrictions
        }
        fld tbyte[d]                   // d
        fldl2e                         // l2e                   d
        fmulp st(1), st                // l2e*d
        fld st(0)                      // l2e*d                 l2e*d
        frndint                        // round(l2e*d)          l2e*d
        fxch st(1)                     // l2e*d                 round(l2e*d)
        fsub st, st(1)                 // l2e*d-round(l2e*d)    round(l2e*d)
        ftst                           // l2e*d-round(l2e*d)<0?
        fstsw sw1
        mov ah, [sw1 + 1]
        sahf
        jb @@negative

        f2xm1                          // 2**(l2e*d-round(l2e*d))-1   round(l2e*d)
        fld1                           // 1 2**(l2e*d-round(l2e*d))-1 round(l2e*d)
        faddp st(1), st                // 2**(l2e*d-round(l2e*d))     round(l2e*d)
        jmp @@common

@@negative:
        fchs                           // -l2e*d+round(l2e*d)           round(l2e*d)
        f2xm1                          // 2**(-l2e*d+round(l2e*d))-1    round(l2e*d)
        fld1                           // 1  2**(-l2e*d+round(l2e*d))-1 round(l2e*d)
        fadd st(1), st                 // 1  2**(-l2e*d+round(l2e*d))   round(l2e*d)
        fdivrp st(1), st               // 2**(l2e*d-round(l2e*d))       round(l2e*d)

@@common:
        fscale                         // (2**(l2e*d-round(l2e*d)))*(2**round(l2e*d))  round(l2e*d)
        fstp st(1)                     // (2**(l2e*d-round(l2e*d)))*(2**round(l2e*d))
     end;

    {$define FPC_SYSTEM_HAS_FRAC}
    function fpc_frac_real(d : ValReal) : ValReal;assembler;compilerproc;
      asm
        sub sp, 2
        fnstcw [bp-2]
        fwait
        mov cx, [bp-2]
        or word [bp-2], $0f00
        fldcw [bp-2]
        fld tbyte [d]
        frndint
        fld tbyte [d]
        fsub st, st(1)
        fstp st(1)
        mov [bp-2], cx
        fldcw [bp-2]
      end;

    {$define FPC_SYSTEM_HAS_INT}
    function fpc_int_real(d : ValReal) : ValReal;assembler;compilerproc;
      asm
        sub sp, 2
        fnstcw [bp-2]
        fwait
        mov cx, word [bp-2]
        or word [bp-2], $0f00
        fldcw [bp-2]
        fwait
        fld tbyte [d]
        frndint
        fwait
        mov word [bp-2], cx
        fldcw [bp-2]
      end;

    {$define FPC_SYSTEM_HAS_TRUNC}
    function fpc_trunc_real(d : ValReal) : int64;assembler;compilerproc;
      asm
        sub sp, 10
        fld tbyte [d]
        fnstcw [bp-10]
        mov cx, [bp-10]
        or word [bp-10], $0f00
        fldcw [bp-10]
        mov [bp-10], cx
        fistp qword [bp-8]
        fldcw [bp-10]
        fwait
        mov dx, [bp-8]
        mov cx, [bp-6]
        mov bx, [bp-4]
        mov ax, [bp-2]
      end;

