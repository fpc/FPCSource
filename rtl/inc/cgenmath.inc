{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by Several contributors

    Generic mathemtical routines in libc

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ for 80x86, we can easily write the optimal inline code }
{ Furthermore, the routines below only go up to double   }
{ precision and we need extended precision if supported  }
{$ifndef FPC_HAS_TYPE_EXTENDED}

{$ifndef SOLARIS}

{$ifndef FPC_SYSTEM_HAS_INT}
{$define FPC_SYSTEM_HAS_INT}

{$ifdef SUPPORT_DOUBLE}
    function c_trunc(d: double): double; cdecl; external 'c' name 'trunc';

    function fpc_int_real(d: ValReal): ValReal;compilerproc; {$ifdef MATHINLINE}inline;{$endif}
    begin
      result := c_trunc(d);
    end;


{$else SUPPORT_DOUBLE}

    function c_truncf(d: double): double; cdecl; external 'c' name 'truncf';

    function fpc_int_real(d: ValReal): ValReal;compilerproc; {$ifdef MATHINLINE}inline;{$endif}
      begin
        { this will be correct since real = single in the case of }
        { the motorola version of the compiler...                 }
        int:=c_truncf(d);
      end;
{$endif SUPPORT_DOUBLE}

{$endif FPC_SYSTEM_HAS_INT}
{$endif SOLARIS}

{$ifndef SYSTEM_HAS_FREXP}
{$define SYSTEM_HAS_FREXP}
    function c_frexp(x: double; out e: longint): double; cdecl; external 'c' name 'frexp';

    function frexp(x:ValReal; out e:Integer ):ValReal; {$ifdef MATHINLINE}inline;{$endif}
    var
      l: longint;
    begin
      frexp := c_frexp(x,l);
      e := l;
    end;
{$endif not SYSTEM_HAS_FREXP}


{$ifndef SYSTEM_HAS_LDEXP}
{$define SYSTEM_HAS_LDEXP}
    function c_ldexp(x: double; n: longint): double; cdecl; external 'c' name 'ldexp';

    function ldexp( x: ValReal; N: Integer):ValReal;{$ifdef MATHINLINE}inline;{$endif}
    begin
      ldexp := c_ldexp(x,n);
    end;
{$endif not SYSTEM_HAS_LDEXP}


{$ifndef FPC_SYSTEM_HAS_SQRT}
{$define FPC_SYSTEM_HAS_SQRT}

    function c_sqrt(d: double): double; cdecl; external 'c' name 'sqrt';

    function fpc_sqrt_real(d:ValReal):ValReal;compilerproc; {$ifdef MATHINLINE}inline;{$endif}
    begin
      result := c_sqrt(d);
    end;

{$endif}


{$ifndef FPC_SYSTEM_HAS_EXP}
{$define FPC_SYSTEM_HAS_EXP}
    function c_exp(d: double): double; cdecl; external 'c' name 'exp';

    function fpc_Exp_real(d:ValReal):ValReal;compilerproc; {$ifdef MATHINLINE}inline;{$endif}
    begin
      result := c_exp(d);
    end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_LN}
{$define FPC_SYSTEM_HAS_LN}

    function c_log(d: double): double; cdecl; external 'c' name 'log';

    function fpc_Ln_real(d:ValReal):ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
    begin
      result := c_log(d);
    end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_SIN}
{$define FPC_SYSTEM_HAS_SIN}
    function c_sin(d: double): double; cdecl; external 'c' name 'sin';

    function fpc_Sin_real(d:ValReal):ValReal;compilerproc; {$ifdef MATHINLINE}inline;{$endif}
    begin
      result := c_sin(d);
    end;
{$endif}



{$ifndef FPC_SYSTEM_HAS_COS}
{$define FPC_SYSTEM_HAS_COS}
    function c_cos(d: double): double; cdecl; external 'c' name 'cos';

    function fpc_Cos_real(d:ValReal):ValReal;compilerproc; {$ifdef MATHINLINE}inline;{$endif}
    begin
      result := c_cos(d);
    end;
{$endif}



{$ifndef FPC_SYSTEM_HAS_ARCTAN}
{$define FPC_SYSTEM_HAS_ARCTAN}
    function c_atan(d: double): double; cdecl; external 'c' name 'atan';

    function fpc_ArcTan_real(d:ValReal):ValReal;compilerproc; {$ifdef MATHINLINE}inline;{$endif}
    begin
      result := c_atan(d);
    end;
{$endif}

{$endif not FPC_HAS_TYPE_EXTENDED}

