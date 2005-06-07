{
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

    function pi : ValReal;[internproc:fpc_in_pi_real];
    function abs(d : ValReal) : ValReal;[internproc:fpc_in_abs_real];
    function sqr(d : ValReal) : ValReal;[internproc:fpc_in_sqr_real];
    function sqrt(d : ValReal) : ValReal;[internproc:fpc_in_sqrt_real];
    function arctan(d : ValReal) : ValReal;[internproc:fpc_in_arctan_real];
    function ln(d : ValReal) : ValReal;[internproc:fpc_in_ln_real];
    function sin(d : ValReal) : ValReal;[internproc:fpc_in_sin_real];
    function cos(d : ValReal) : ValReal;[internproc:fpc_in_cos_real];
    function exp(d : ValReal) : ValReal;[internproc:fpc_in_exp_real];
    function round(d : ValReal) : int64;[internproc:fpc_in_round_real];
    function frac(d : ValReal) : ValReal;[internproc:fpc_in_frac_real];
    function int(d : ValReal) : ValReal;[internproc:fpc_in_int_real];
    function trunc(d : ValReal) : int64;[internproc:fpc_in_trunc_real];

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

