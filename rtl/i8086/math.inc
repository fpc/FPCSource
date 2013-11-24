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
        cw1,cw2: word;
      asm
        // comes from DJ GPP
        fld tbyte[d]
        fldl2e
        fmulp st(1), st
        fstcw CW1
        fstcw CW2
        fwait
        and CW2, $f3ff
        or CW2, $0400
        fldcw CW2
        fld st(0)
        frndint
        fldcw CW1
        fxch st(1)
        fsub st, st(1)
        f2xm1
        fld1
        faddp st(1), st
        fscale
        fstp st(1)
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

