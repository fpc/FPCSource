{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    Implementation of mathematical Routines (only for real)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef WINCE}
    {$define FPC_SYSTEM_HAS_ABS}
    function fpc_abs_real(d : extended) : extended;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$define FPC_SYSTEM_HAS_SQR}
    function fpc_sqr_real(d : extended) : extended;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    {$define FPC_SYSTEM_HAS_SQRT}
    function fpc_sqrt_real(d : extended) : extended;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    (* atn isn't supported by the linux fpe it seems
    {$define FPC_SYSTEM_HAS_ARCTAN}
    function fpc_arctan_real(d : extended) : extended;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    *)
    (* lgn isn't supported by the linux fpe it seems
    {$define FPC_SYSTEM_HAS_LN}
    function fpc_ln_real(d : extended) : extended;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    *)
    (* sind isn't supported by the linux fpe it seems
    {$define FPC_SYSTEM_HAS_SIN}
    function fpc_sin_real(d : extended) : extended;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    *)
    (* cos isn't supported by the linux fpe it seems
    {$define FPC_SYSTEM_HAS_COS}
    function fpc_cos_real(d : extended) : extended;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;
    *)
{$endif WINCE}
