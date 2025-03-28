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
    {$define FPC_SYSTEM_FPU_CUSTOM_CONTROL_WORD}
    const
      Default8087CW : word = $1332;

    procedure Set8087CW(cw:word);
    function Get8087CW:word;
{$endif}

{$if defined (cpui386) or defined(cpux86_64)}
    {$define FPC_SYSTEM_FPU_CUSTOM_CONTROL_WORD}
    const
      DefaultMXCSR: dword = $1900;

    procedure SetMXCSR(w: dword);
    function GetMXCSR: dword;
    procedure SetSSECSR(w : dword); deprecated 'Renamed to SetMXCSR';
    function GetSSECSR : dword; deprecated 'Renamed to GetMXCSR';
{$endif}

{$if defined(cpum68k)}
    {$define FPC_SYSTEM_FPU_CUSTOM_CONTROL_WORD}
{$if defined(fpu68881) or defined(fpucoldfire)}
    const
    {$ifdef FPC_68K_SYSTEM_HAS_FPU_EXCEPTIONS}
       Default68KFPCR: DWord = $3400; { Enable OVFL, OPERR and DZ, round to nearest, default precision }
    {$else}
       Default68KFPCR: DWord = 0;
    {$endif}

    procedure SetFPCR(x: DWord);
    procedure SetFPSR(x: DWord);
    function GetFPCR: DWord;
    function GetFPSR: DWord;
{$else}
    {$define FPC_SYSTEM_FPUCW_IMMUTABLE}
{$endif}
{$endif}

{$if (defined(FPC_SYSTEM_FPUCW_IMMUTABLE) or defined(FPU_NONE)) and not declared(TNativeFPUControlWord)}
  type
    TNativeFPUControlWord = record
    end;
{$endif}

    function GetNativeFPUControlWord: TNativeFPUControlWord; {$if (defined(FPC_SYSTEM_FPUCW_IMMUTABLE) or defined(FPUNONE)) and defined(SYSTEMINLINE)}inline;{$endif}
    procedure SetNativeFPUControlWord(const cw: TNativeFPUControlWord); {$if (defined(FPC_SYSTEM_FPUCW_IMMUTABLE) or defined(FPUNONE)) and defined(SYSTEMINLINE)}inline;{$endif}

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
  softfloat_exception_flags : TFPUExceptionMask;
  softfloat_exception_mask : TFPUExceptionMask;
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

    function Pi : ValReal;[internproc:fpc_in_pi_real];
    function Abs(d : ValReal) : ValReal;[internproc:fpc_in_abs_real];
    function Sqr(d : ValReal) : ValReal;[internproc:fpc_in_sqr_real];
    function Sqrt(d : ValReal) : ValReal;[internproc:fpc_in_sqrt_real];
    function ArcTan(d : ValReal) : ValReal;[internproc:fpc_in_arctan_real];
    function Ln(d : ValReal) : ValReal;[internproc:fpc_in_ln_real];
    function Sin(d : ValReal) : ValReal;[internproc:fpc_in_sin_real];
    function Cos(d : ValReal) : ValReal;[internproc:fpc_in_cos_real];
    function Exp(d : ValReal) : ValReal;[internproc:fpc_in_exp_real];
    function Round(d : ValReal) : int64;[internproc:fpc_in_round_real];
    function Frac(d : ValReal) : ValReal;[internproc:fpc_in_frac_real];
    function Int(d : ValReal) : ValReal;[internproc:fpc_in_int_real];
    function Trunc(d : ValReal) : int64;[internproc:fpc_in_trunc_real];

{$ifdef SUPPORT_EXTENDED}
    function FPower10(val: Extended; Power: Longint): Extended;
{$endif SUPPORT_EXTENDED}

    type
       Real48 = array[0..5] of byte;

{$ifdef SUPPORT_DOUBLE}
    function Real2Double(r : real48) : double;
    operator := (b:real48) d:double; {$ifdef SYSTEMINLINE}inline;{$endif}
{$endif}
{$ifdef SUPPORT_EXTENDED}
    operator := (b:real48) e:extended; {$ifdef SYSTEMINLINE}inline;{$endif}
{$endif SUPPORT_EXTENDED}

    type
      TFloatSpecial = (fsZero,fsNZero,fsDenormal,fsNDenormal,fsPositive,fsNegative,
                       fsInf,fsNInf,fsNaN,fsInvalidOp);

{$if defined(SUPPORT_EXTENDED) or defined(FPC_SOFT_FPUX80)}
      TExtended80Rec = packed record
      private
      const
        Bias = $3FFF;
        function GetExp : QWord;
        procedure SetExp(e : QWord);
        function GetSign : Boolean;
        procedure SetSign(s : Boolean);
      public
        function Mantissa(IncludeHiddenBit: Boolean = False) : QWord; // unused parameter inserted to have consistent function signature
        function Fraction : Extended;
        function Exponent : Longint;
        property Sign : Boolean read GetSign write SetSign;
        property Exp : QWord read GetExp write SetExp;
        function SpecialType : TFloatSpecial;
        procedure BuildUp(const _Sign : Boolean; const _Mantissa : QWord; const _Exponent : Longint);
        case byte of
          0: (Bytes : array[0..9] of Byte);
          1: (Words : array[0..4] of Word);
{$ifdef ENDIAN_LITTLE}
          2: (Frac : QWord; _Exp: Word);
{$else ENDIAN_LITTLE}
          2: (_Exp: Word; Frac : QWord);
{$endif ENDIAN_LITTLE}
{$ifdef SUPPORT_EXTENDED}
          3: (Value: Extended);
{$else}
          3: (Value: array[0..9] of Byte);
{$endif}
      end;
{$endif SUPPORT_EXTENDED or FPC_SOFT_FPUX80}

{$ifdef SUPPORT_DOUBLE}
      TDoubleRec = packed record
      private
      const
        Bias = $3FF;
        function GetExp : QWord;
        procedure SetExp(e : QWord);
        function GetSign : Boolean;
        procedure SetSign(s : Boolean);
        function GetFrac : QWord;
        procedure SetFrac(e : QWord);
      public
        function Mantissa(IncludeHiddenBit: Boolean = False) : QWord;
        function Fraction : ValReal;
        function Exponent : Longint;
        property Sign : Boolean read GetSign write SetSign;
        property Exp : QWord read GetExp write SetExp;
        property Frac : QWord read Getfrac write SetFrac;
        function SpecialType : TFloatSpecial;
        procedure BuildUp(const _Sign : Boolean; const _Mantissa : QWord; const _Exponent : Longint);
        case byte of
          0: (Bytes : array[0..7] of Byte);
          1: (Words : array[0..3] of Word);
          2: (Data : QWord);
          3: (Value: Double);
      end;
{$endif SUPPORT_DOUBLE}

{$ifdef SUPPORT_SINGLE}
      TSingleRec = packed record
      private
      const
        Bias = $7F;
        function GetExp : QWord;
        procedure SetExp(e : QWord);
        function GetSign : Boolean;
        procedure SetSign(s : Boolean);
        function GetFrac : QWord;
        procedure SetFrac(e : QWord);
      public
        function Mantissa(IncludeHiddenBit: Boolean = False) : QWord;
        function Fraction : ValReal;
        function Exponent : Longint;
        property Sign : Boolean read GetSign write SetSign;
        property Exp : QWord read GetExp write SetExp;
        property Frac : QWord read Getfrac write SetFrac;
        function SpecialType : TFloatSpecial;
        procedure BuildUp(const _Sign : Boolean; const _Mantissa : QWord; const _Exponent : Longint);
        case byte of
          0: (Bytes : array[0..3] of Byte);
          1: (Words : array[0..1] of Word);
          2: (Data : DWord);
          3: (Value: Single);
      end;
{$endif SUPPORT_SINGLE}

    function FMASingle(s1,s2,s3 : single) : single;[internproc:fpc_in_fma_single];
{$ifdef SUPPORT_DOUBLE}
    function FMADouble(d1,d2,d3 : double) : double;[internproc:fpc_in_fma_double];
{$endif SUPPORT_DOUBLE}
{$ifdef SUPPORT_EXTENDED}
    function FMAExtended(e1,e2,e3 : extended) : extended;[internproc:fpc_in_fma_extended];
{$endif SUPPORT_EXTENDED}
{$ifdef SUPPORT_FLOAT128}
    function FMAFloat128(f1,f2,f3 : float128) : float128;[internproc:fpc_in_fma_float128];
{$endif SUPPORT_FLOAT128}

