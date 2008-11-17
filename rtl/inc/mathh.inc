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

{$if defined(cpui386) or defined(cpux86_64)}
    const
      Default8087CW : word = $1332;

    procedure Set8087CW(cw:word);
    function Get8087CW:word;
    procedure SetSSECSR(w : dword);
    function GetSSECSR : dword;
{$endif}

const
{*
-------------------------------------------------------------------------------
Software IEC/IEEE floating-point exception flags.
-------------------------------------------------------------------------------
*}
    float_flag_invalid   =  1;
    float_flag_denormal  =  2;
    float_flag_divbyzero =  4;
    float_flag_overflow  =  8;
    float_flag_underflow = 16;
    float_flag_inexact   = 32;

{*
-------------------------------------------------------------------------------
Software IEC/IEEE floating-point rounding mode.
-------------------------------------------------------------------------------
*}
    float_round_nearest_even = 0;
    float_round_down         = 1;
    float_round_up           = 2;
    float_round_to_zero      = 3;

threadvar
  softfloat_exception_mask : Byte;
  softfloat_exception_flags : Byte;
  softfloat_rounding_mode : Byte;

procedure float_raise(i: shortint);

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

{$ifdef SUPPORT_EXTENDED}
    function FPower10(val: Extended; Power: Longint): Extended;
{$endif SUPPORT_EXTENDED}

    type
       real48 = array[0..5] of byte;

{$ifdef SUPPORT_DOUBLE}
    function Real2Double(r : real48) : double;
    operator := (b:real48) d:double;
{$endif}
{$ifdef SUPPORT_EXTENDED}
    operator := (b:real48) e:extended;
{$endif SUPPORT_EXTENDED}

