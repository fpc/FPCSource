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

{$if defined(cpui8086) or defined(cpui386) or defined(cpux86_64)}
    const
      Default8087CW : word = $1332;

    procedure Set8087CW(cw:word);
    function Get8087CW:word;
  {$ifndef cpui8086}
    procedure SetSSECSR(w : dword);
    function GetSSECSR : dword;
  {$endif not cpui8086}
{$endif}

  type
    TFPURoundingMode = (rmNearest, rmDown, rmUp, rmTruncate);
    TFPUPrecisionMode = (pmSingle, pmReserved, pmDouble, pmExtended);
    TFPUException = (exInvalidOp, exDenormalized, exZeroDivide,
                     exOverflow, exUnderflow, exPrecision);
    TFPUExceptionMask = set of TFPUException;

const
{*
-------------------------------------------------------------------------------
Software IEC/IEEE floating-point exception flags.
-------------------------------------------------------------------------------
*}
    float_flag_invalid   =  exInvalidOp;
    float_flag_denormal  =  exDenormalized;
    float_flag_divbyzero =  exZeroDivide;
    float_flag_overflow  =  exOverflow;
    float_flag_underflow =  exUnderflow;
    float_flag_inexact   =  exPrecision;

{*
-------------------------------------------------------------------------------
Software IEC/IEEE floating-point rounding mode.
-------------------------------------------------------------------------------
*}
    float_round_nearest_even = rmNearest;
    float_round_down         = rmDown;
    float_round_up           = rmUp;
    float_round_to_zero      = rmTruncate;

{$ifdef FPC_HAS_FEATURE_THREADING}
ThreadVar
{$else FPC_HAS_FEATURE_THREADING}
Var
{$endif FPC_HAS_FEATURE_THREADING}
  softfloat_exception_mask : TFPUExceptionMask;
  softfloat_exception_flags : TFPUExceptionMask;
  softfloat_rounding_mode : TFPURoundingMode;

procedure float_raise(i: TFPUException);
procedure float_raise(i: TFPUExceptionMask);

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


    function fma(s1,s2,s3 : single) : single;[internproc:fpc_in_fma_single];
{$ifdef SUPPORT_DOUBLE}
    function fma(d1,d2,d3 : double) : double;[internproc:fpc_in_fma_double];
{$endif SUPPORT_DOUBLE}
{$ifdef SUPPORT_EXTENDED}
    function fma(e1,e2,e3 : extended) : extended;[internproc:fpc_in_fma_extended];
{$endif SUPPORT_EXTENDED}
{$ifdef SUPPORT_FLOAT128}
    function fma(f1,f2,f3 : float128) : float128;[internproc:fpc_in_fma_float128];
{$endif SUPPORT_FLOAT128}

