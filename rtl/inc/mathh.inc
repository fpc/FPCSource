{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl,
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

   { i386 FPU Controlword }

{$ifdef cpui386}
    const
      Default8087CW : word = $1332;

    procedure Set8087CW(cw:word);
    function Get8087CW:word;
{$endif cpui386}

{$ifdef cpux86_64}
    const
      Default8087CW : word = $1332;

    procedure Set8087CW(cw:word);
    function Get8087CW:word;
{$endif cpux86_64}

   { declarations of the math routines }

{$ifdef cpui386}
  {$define INTERNMATH}
{$endif}

{$ifndef INTERNMATH}
  {$ifdef FPC_USE_LIBC}
    {$ifdef SYSTEMINLINE}
      {$define MATHINLINE}
    {$endif}
  {$endif}
{$endif}

{$ifdef internconstintf}
    function pi : ValReal;[internproc:fpc_in_pi_real];
    function abs(d : ValReal) : ValReal;[internproc:fpc_in_abs_real];
    function sqr(d : ValReal) : ValReal;[internproc:fpc_in_sqr_real];
    function sqrt(d : ValReal) : ValReal;[internproc:fpc_in_sqrt_real];
    function arctan(d : ValReal) : ValReal;[internproc:fpc_in_sqr_real];
    function ln(d : ValReal) : ValReal;[internproc:fpc_in_ln_real];
    function sin(d : ValReal) : ValReal;[internproc:fpc_in_sin_real];
    function cos(d : ValReal) : ValReal;[internproc:fpc_in_cos_real];
    function exp(d : ValReal) : ValReal;[internproc:fpc_in_exp_real];
    function round(d : ValReal) : int64;[internproc:fpc_in_round_real];
    function frac(d : ValReal) : ValReal;[internproc:fpc_in_frac_real];
    function int(d : ValReal) : ValReal;[internproc:fpc_in_int_real];
    function trunc(d : ValReal) : int64;[internproc:fpc_in_trunc_real];
{$else}
    function abs(d : ValReal) : ValReal;
    function arctan(d : ValReal) : ValReal;{$ifdef MATHINLINE}inline;{$endif}
    function cos(d : ValReal) : ValReal;{$ifdef MATHINLINE}inline;{$endif}
    function exp(d : ValReal) : ValReal;{$ifdef MATHINLINE}inline;{$endif}
    function frac(d : ValReal) : ValReal;
    function int(d : ValReal) : ValReal;{$ifdef MATHINLINE}inline;{$endif}
    function ln(d : ValReal) : ValReal;{$ifdef MATHINLINE}inline;{$endif}
    function pi : ValReal;
    function sin(d : ValReal) : ValReal;{$ifdef MATHINLINE}inline;{$endif}
    function sqr(d : ValReal) : ValReal;
    function sqrt(d : ValReal) : ValReal;{$ifdef MATHINLINE}inline;{$endif}
    function round(d : ValReal) : int64;
    function trunc(d : ValReal) : int64;
{$endif internconstintf}

    function power(bas,expo : ValReal) : ValReal;
    function power(bas,expo : int64) : int64;

{$ifdef FPC_CURRENCY_IS_INT64}
    function trunc(c : currency) : int64;
    function trunc(c : comp) : int64;
    function round(c : currency) : int64;
    function round(c : comp) : int64;
{$endif FPC_CURRENCY_IS_INT64}


    type
       real48 = array[0..5] of byte;

{$ifdef SUPPORT_DOUBLE}
    function Real2Double(r : real48) : double;
    operator := (b:real48) d:double;
{$endif}
{$ifdef SUPPORT_EXTENDED}
    operator := (b:real48) e:extended;
{$endif SUPPORT_EXTENDED}

{
  $Log$
  Revision 1.19  2004-11-21 15:35:23  peter
    * float routines all use internproc and compilerproc helpers

  Revision 1.18  2004/11/18 10:03:36  michael
  + Patch from peter to fix pasjpeg compile

  Revision 1.17  2004/11/17 22:19:04  peter
  internconst, internproc and some external declarations moved to interface

  Revision 1.16  2004/10/09 21:00:46  jonas
    + cgenmath with libc math functions. Faster than the routines in genmath
      and also have full double support (exp() only has support for values in
      the single range in genmath, for example). Used in FPC_USE_LIBC is
      defined
    * several fixes to allow compilation with -dHASINLINE, but internalerrors
      because of missing support for inlining assembler code

  Revision 1.15  2004/02/08 15:33:50  florian
    * linking problems fixed
    + abi tag added

  Revision 1.14  2004/01/02 17:19:04  jonas
    * if currency = int64, FPC_CURRENCY_IS_INT64 is defined
    + round and trunc for currency and comp if FPC_CURRENCY_IS_INT64 is
      defined
    * if currency = orddef, prefer currency -> int64/qword conversion over
      currency -> float conversions
    * optimized currency/currency if currency = orddef
    * TODO: write FPC_DIV_CURRENCY and FPC_MUL_CURRENCY routines to prevent
        precision loss if currency=int64 and bestreal = double

  Revision 1.13  2003/01/21 19:36:36  mazen
  - fpc_int64_to_double removed as not supported by most cpu targets

  Revision 1.12  2003/01/20 22:21:36  mazen
  * many stuff related to RTL fixed

  Revision 1.11  2003/01/15 00:40:18  peter
    * power returns int64

  Revision 1.10  2003/01/03 20:34:02  peter
    * i386 fpu controlword functions added

  Revision 1.9  2002/10/06 21:26:18  peter
    * round returns int64

  Revision 1.8  2002/09/07 15:07:45  peter
    * old logs removed and tabs fixed

  Revision 1.7  2002/07/26 22:46:06  florian
    * interface of system unit for Linux/PowerPC compiles

}
