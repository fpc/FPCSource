{
    $Id$
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
{$ifndef cpui386}

{$ifndef FPC_SYSTEM_HAS_INT}
{$define FPC_SYSTEM_HAS_INT}

{$ifdef SUPPORT_DOUBLE}
    function c_trunc(d: double): double; cdecl; external 'c' name 'trunc';
    
    function int(d: double): double; {$ifdef MATHINLINE}inline;{$endif} [internconst:in_const_int];
    begin
      result := c_trunc(d);
    end;


{$else SUPPORT_DOUBLE}

    function c_truncf(d: real): double; cdecl; external 'c' name 'truncf';
    
    function int(d: real): real; {$ifdef MATHINLINE}inline; dsfqsdfqs{$endif}
    begin
      result := c_truncf(d);
    end;
    
    function int(d: real) : real;[internconst:in_const_int];
      begin
        { this will be correct since real = single in the case of }
        { the motorola version of the compiler...                 }
        int:=c_truncf(d);
      end;
{$endif SUPPORT_DOUBLE}

{$endif}


{$ifndef SYSTEM_HAS_FREXP}
{$define SYSTEM_HAS_FREXP}
    function c_frexp(x: double; var e: longint): double; cdecl; external 'c' name 'frexp';

    function frexp(x:Real; var e:Integer ):Real; {$ifdef MATHINLINE}inline;{$endif}
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

    function ldexp( x: Real; N: Integer):Real;{$ifdef MATHINLINE}inline;{$endif}
    begin
      ldexp := c_ldexp(x,n);
    end;
{$endif not SYSTEM_HAS_LDEXP}


{$ifndef FPC_SYSTEM_HAS_SQRT}
{$define FPC_SYSTEM_HAS_SQRT}

    function c_sqrt(d: double): double; cdecl; external 'c' name 'sqrt';

    function sqrt(d:Real):Real;[internconst:in_const_sqrt]; [public, alias: 'FPC_SQRT_REAL']; {$ifdef MATHINLINE}inline;{$endif}
    begin
      sqrt := c_sqrt(d);
    end;

{$ifdef hascompilerproc}
    function fpc_sqrt_real(d:Real):Real;compilerproc; external name 'FPC_SQRT_REAL';
{$endif hascompilerproc}

{$endif}


{$ifndef FPC_SYSTEM_HAS_EXP}
{$define FPC_SYSTEM_HAS_EXP}
    function c_exp(d: double): double; cdecl; external 'c' name 'exp';

    function Exp(d:Real):Real;[internconst:in_const_exp]; {$ifdef MATHINLINE}inline;{$endif}
    begin
      exp := c_exp(d);
    end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_ROUND}
{$define FPC_SYSTEM_HAS_ROUND}

    function c_llround(d: double): int64; cdecl; external 'c' name 'llround';

{$ifdef hascompilerproc}
    function round(d : Real) : int64;[internconst:in_const_round, external name 'FPC_ROUND'];

    function fpc_round(d : Real) : int64;[public, alias:'FPC_ROUND'];{$ifdef hascompilerproc}compilerproc;{$endif hascompilerproc}
    begin
      fpc_round := c_llround(d);
    end;
{$else}
    function round(d : Real) : int64;[internconst:in_const_round];
    begin
      round := c_llround(d);
    end;
{$endif hascompilerproc}
{$endif}



{$ifndef FPC_SYSTEM_HAS_LN}
{$define FPC_SYSTEM_HAS_LN}

    function c_log(d: double): double; cdecl; external 'c' name 'log';

    function Ln(d:Real):Real;[internconst:in_const_ln];{$ifdef MATHINLINE}inline;{$endif}
    begin
      ln := c_log(d);
    end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_SIN}
{$define FPC_SYSTEM_HAS_SIN}
    function c_sin(d: double): double; cdecl; external 'c' name 'sin';

    function Sin(d:Real):Real;[internconst:in_const_sin]; {$ifdef MATHINLINE}inline;{$endif}
    begin
      sin := c_sin(d);
    end;
{$endif}



{$ifndef FPC_SYSTEM_HAS_COS}
{$define FPC_SYSTEM_HAS_COS}
    function c_cos(d: double): double; cdecl; external 'c' name 'cos';

    function Cos(d:Real):Real;[internconst:in_const_cos]; {$ifdef MATHINLINE}inline;{$endif}
    begin
      cos := c_cos(d);
    end;
{$endif}



{$ifndef FPC_SYSTEM_HAS_ARCTAN}
{$define FPC_SYSTEM_HAS_ARCTAN}
    function c_atan(d: double): double; cdecl; external 'c' name 'atan';

    function ArcTan(d:Real):Real;[internconst:in_const_arctan]; {$ifdef MATHINLINE}inline;{$endif}
    begin
      arctan := c_atan(d);
    end;
{$endif}

{$endif not i386}

{
  $Log$
  Revision 1.1  2004-10-09 21:00:46  jonas
    + cgenmath with libc math functions. Faster than the routines in genmath
      and also have full double support (exp() only has support for values in
      the single range in genmath, for example). Used in FPC_USE_LIBC is
      defined
    * several fixes to allow compilation with -dHASINLINE, but internalerrors
      because of missing support for inlining assembler code

}
