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
  {$ifdef INTERNMATH}
    {$define FPC_SYSTEM_HAS_PI}
    function pi : extended;[internproc:fpc_in_pi];
    {$define FPC_SYSTEM_HAS_ABS}
    function abs(d : extended) : extended;[internproc:fpc_in_abs_extended];
    {$define FPC_SYSTEM_HAS_SQR}
    function sqr(d : extended) : extended;[internproc:fpc_in_sqr_extended];
    {$define FPC_SYSTEM_HAS_SQRT}
    function sqrt(d : extended) : extended;[internproc:fpc_in_sqrt_extended];
    {$define FPC_SYSTEM_HAS_ARCTAN}
    function arctan(d : extended) : extended;[internproc:fpc_in_sqr_extended];
    {$define FPC_SYSTEM_HAS_LN}
    function ln(d : extended) : extended;[internproc:fpc_in_ln_extended];
    {$define FPC_SYSTEM_HAS_SIN}
    function sin(d : extended) : extended;[internproc:fpc_in_sin_extended];
    {$define FPC_SYSTEM_HAS_COS}
    function cos(d : extended) : extended;[internproc:fpc_in_cos_extended];
    function exp(d : extended) : extended;[internconst:fpc_in_const_exp];

    function round(d : extended) : int64;[internconst:fpc_in_const_exp];external name 'FPC_ROUND';
    function frac(d : extended) : extended;[internconst:fpc_in_const_frac];
    function int(d : extended) : extended;[internconst:fpc_in_const_int];
    function trunc(d : extended) : int64;[internconst:fpc_in_const_trunc];
  {$else}
    function abs(d : extended) : extended;[internconst:fpc_in_abs_extended];
    function arctan(d : extended) : extended;[internconst:fpc_in_arctan_extended];{$ifdef MATHINLINE}inline;{$endif}
    function cos(d : extended) : extended;[internconst:fpc_in_sqr_extended];{$ifdef MATHINLINE}inline;{$endif}
    function exp(d : extended) : extended;[internconst:fpc_in_const_exp];{$ifdef MATHINLINE}inline;{$endif}
    function frac(d : extended) : extended;[internconst:fpc_in_const_frac];
    function int(d : extended) : extended;[internconst:fpc_in_const_int];{$ifdef MATHINLINE}inline;{$endif}
    function ln(d : extended) : extended;[internconst:fpc_in_ln_extended];{$ifdef MATHINLINE}inline;{$endif}
    function pi : extended; [internconst:fpc_in_pi];
    function sin(d : extended) : extended;[internconst:fpc_in_sin_extended];{$ifdef MATHINLINE}inline;{$endif}
    function sqr(d : extended) : extended;[internconst:fpc_in_sqr_extended];
    function sqrt(d : extended) : extended;[internconst:fpc_in_sqrt_extended];{$ifdef MATHINLINE}inline;{$endif}
    function round(d : extended) : int64;[internconst:fpc_in_const_exp];
    function trunc(d : extended) : int64;[internconst:fpc_in_const_trunc];
  {$endif}
{$else}
    function abs(d : extended) : extended;
    function arctan(d : extended) : extended;{$ifdef MATHINLINE}inline;{$endif}
    function cos(d : extended) : extended;{$ifdef MATHINLINE}inline;{$endif}
    function exp(d : extended) : extended;{$ifdef MATHINLINE}inline;{$endif}
    function frac(d : extended) : extended;
    function int(d : extended) : extended;{$ifdef MATHINLINE}inline;{$endif}
    function ln(d : extended) : extended;{$ifdef MATHINLINE}inline;{$endif}
    function pi : extended;
    function sin(d : extended) : extended;{$ifdef MATHINLINE}inline;{$endif}
    function sqr(d : extended) : extended;
    function sqrt(d : extended) : extended;{$ifdef MATHINLINE}inline;{$endif}
    function round(d : extended) : int64;
    function trunc(d : extended) : int64;
{$endif internconstintf}

    function power(bas,expo : extended) : extended;
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
  Revision 1.17  2004-11-17 22:19:04  peter
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
