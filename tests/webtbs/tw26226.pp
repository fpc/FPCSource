{ %norun }

{ *********************************************************** }
{ *       Numerics(32/64).dll import for Pascal             * }
{ * ------------------------------------------------------- * }
{ *       set const LibName = 'numerics32.dll'              * }
{ *         to import numerics32.dll;                       * }
{ *       set const LibName = 'numerics64.dll'              * }
{ *         to import numerics64.dll;                       * }
{ *********************************************************** }

{
  Copyright (c) 2013 Sergey Kasandrov

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
}

unit tw26266;

{$mode delphi}

interface

uses SysUtils;

type
  TF_RESULT = LongInt;

const
                                              // = common microsoft codes =
  TF_S_OK           = TF_RESULT(0);           // Operation successful
  TF_S_FALSE        = TF_RESULT(1);           // Operation successful
  TF_E_FAIL         = TF_RESULT($80004005);   // Unspecified failure
  TF_E_INVALIDARG   = TF_RESULT($80070057);   // One or more arguments are not valid
  TF_E_NOINTERFACE  = TF_RESULT($80004002);   // No such interface supported
  TF_E_NOTIMPL      = TF_RESULT($80004001);   // Not implemented
  TF_E_OUTOFMEMORY  = TF_RESULT($8007000E);   // Failed to allocate necessary memory
  TF_E_UNEXPECTED   = TF_RESULT($8000FFFF);   // Unexpected failure

                                              // = TFL specific codes =
  TF_E_NOMEMORY     = TF_RESULT($A0000003);   // specific TFL memory error
  TF_E_LOADERROR    = TF_RESULT($A0000004);   // Error loading dll

{$IFDEF FPC}
type
  TBytes = array of Byte;
{$ENDIF}

type
  IBigNumber = interface

    function GetIsEven: Boolean; stdcall;
    function GetIsOne: Boolean; stdcall;
    function GetIsPowerOfTwo: Boolean; stdcall;
    function GetIsZero: Boolean; stdcall;
    function GetSign: Integer; stdcall;
    function GetSize: Integer; stdcall;

    function CompareNumber(Num: IBigNumber): Integer; stdcall;
    function CompareNumberU(Num: IBigNumber): Integer; stdcall;

    function AddNumber(Num: IBigNumber; var Res: IBigNumber): TF_RESULT; stdcall;
    function AddNumberU(Num: IBigNumber; var Res: IBigNumber): TF_RESULT; stdcall;
    function SubNumber(Num: IBigNumber; var Res: IBigNumber): TF_RESULT; stdcall;
    function SubNumberU(Num: IBigNumber; var Res: IBigNumber): TF_RESULT; stdcall;
    function MulNumber(Num: IBigNumber; var Res: IBigNumber): TF_RESULT; stdcall;
    function MulNumberU(Num: IBigNumber; var Res: IBigNumber): TF_RESULT; stdcall;
    function DivRemNumber(Num: IBigNumber; var Q, R: IBigNumber): TF_RESULT; stdcall;
    function DivRemNumberU(Num: IBigNumber; var Q, R: IBigNumber): TF_RESULT; stdcall;

    function AndNumber(Num: IBigNumber; var Res: IBigNumber): TF_RESULT; stdcall;
    function AndNumberU(Num: IBigNumber; var Res: IBigNumber): TF_RESULT; stdcall;
    function OrNumber(Num: IBigNumber; var Res: IBigNumber): TF_RESULT; stdcall;
    function OrNumberU(Num: IBigNumber; var Res: IBigNumber): TF_RESULT; stdcall;
    function XorNumber(Num: IBigNumber; var Res: IBigNumber): TF_RESULT; stdcall;

    function ShlNumber(Shift: Cardinal; var Res: IBigNumber): TF_RESULT; stdcall;
    function ShrNumber(Shift: Cardinal; var Res: IBigNumber): TF_RESULT; stdcall;

    function AssignNumber(var Res: IBigNumber): TF_RESULT; stdcall;
    function AbsNumber(var Res: IBigNumber): TF_RESULT; stdcall;
    function NegateNumber(var Res: IBigNumber): TF_RESULT; stdcall;
    function Pow(Value: Cardinal; var IRes: IBigNumber): TF_RESULT; stdcall;
    function PowU(Value: Cardinal; var IRes: IBigNumber): TF_RESULT; stdcall;

    function SqrtNumber(var IRes: IBigNumber): TF_RESULT; stdcall;
    function GCD(B: IBigNumber; var G: IBigNumber): TF_RESULT; stdcall;
    function EGCD(B: IBigNumber; var G, X, Y: IBigNumber): TF_RESULT; stdcall;
    function ModPow(IExp, IMod: IBigNumber; var IRes: IBigNumber): TF_RESULT; stdcall;
    function ModInverse(M: IBigNumber; var R: IBigNumber): TF_RESULT; stdcall;

    function ToLimb(var Value: UInt32): TF_RESULT; stdcall;
    function ToIntLimb(var Value: Int32): TF_RESULT; stdcall;
    function ToDec(P: PByte; var L: Integer): TF_RESULT; stdcall;
    function ToHex(P: PByte; var L: Integer; TwoCompl: Boolean): TF_RESULT; stdcall;
    function ToPByte(P: PByte; var L: Cardinal): TF_RESULT; stdcall;

    function CompareToLimb(B: UInt32): Integer; stdcall;
    function CompareToLimbU(B: UInt32): Integer; stdcall;
    function CompareToIntLimb(B: Int32): Integer; stdcall;
    function CompareToIntLimbU(B: Int32): Integer; stdcall;

    function AddLimb(Limb: LongWord; var Res: IBigNumber): TF_RESULT; stdcall;
    function AddLimbU(Limb: LongWord; var Res: IBigNumber): TF_RESULT; stdcall;
    function AddIntLimb(Limb: LongInt; var Res: IBigNumber): TF_RESULT; stdcall;

    function SubLimb(Limb: LongWord; var Res: IBigNumber): TF_RESULT; stdcall;
    function SubLimb2(Limb: LongWord; var Res: IBigNumber): TF_RESULT; stdcall;
    function SubLimbU(Limb: LongWord; var Res: IBigNumber): TF_RESULT; stdcall;
    function SubLimbU2(Limb: LongWord; var Res: LongWord): TF_RESULT; stdcall;
    function SubIntLimb(Limb: LongInt; var Res: IBigNumber): TF_RESULT; stdcall;
    function SubIntLimb2(Limb: LongInt; var Res: IBigNumber): TF_RESULT; stdcall;

    function MulLimb(Limb: LongWord; var Res: IBigNumber): TF_RESULT; stdcall;
    function MulLimbU(Limb: LongWord; var Res: IBigNumber): TF_RESULT; stdcall;
    function MulIntLimb(Limb: LongInt; var Res: IBigNumber): TF_RESULT; stdcall;

    function DivRemLimb(Limb: LongWord; var Q: IBigNumber; var R: IBigNumber): TF_RESULT; stdcall;
    function DivRemLimb2(Limb: LongWord; var Q: IBigNumber; var R: LongWord): TF_RESULT; stdcall;
    function DivRemLimbU(Limb: LongWord; var Q: IBigNumber; var R: LongWord): TF_RESULT; stdcall;
    function DivRemLimbU2(Limb: LongWord; var Q: LongWord; var R: LongWord): TF_RESULT; stdcall;
    function DivRemIntLimb(Limb: LongInt; var Q: IBigNumber; var R: LongInt): TF_RESULT; stdcall;
    function DivRemIntLimb2(Limb: LongInt; var Q: LongInt; var R: LongInt): TF_RESULT; stdcall;

    function ToDblLimb(var Value: UInt64): TF_RESULT; stdcall;
    function ToDblIntLimb(var Value: Int64): TF_RESULT; stdcall;
    function CompareToDblLimb(B: UInt64): Integer; stdcall;
    function CompareToDblLimbU(B: UInt64): Integer; stdcall;
    function CompareToDblIntLimb(B: Int64): Integer; stdcall;
    function CompareToDblIntLimbU(B: Int64): Integer; stdcall;
  end;

type
  BigCardinal = record
  private
    FNumber: IBigNumber;
  public
    function ToString: string;
    function ToHexString(Digits: Integer = 0; const Prefix: string = '';
                         TwoCompl: Boolean = False): string;
    function ToBytes: TBytes;
    function TryParse(const S: string; TwoCompl: Boolean = False): Boolean;
    procedure Free;

    class function Compare(const A, B: BigCardinal): Integer; static;
    function CompareTo(const B: BigCardinal): Integer; overload; inline;

    class function Pow(const Base: BigCardinal; Value: Cardinal): BigCardinal; static;
    class function DivRem(const Dividend, Divisor: BigCardinal;
                          var Remainder: BigCardinal): BigCardinal; overload; static;

    class operator Explicit(const Value: BigCardinal): Cardinal;
    class operator Explicit(const Value: BigCardinal): Integer;
    class operator Explicit(const Value: BigCardinal): UInt64;
    class operator Explicit(const Value: BigCardinal): Int64;
    class operator Implicit(const Value: Cardinal): BigCardinal;
    class operator Implicit(const Value: UInt64): BigCardinal;
    class operator Explicit(const Value: Integer): BigCardinal;
    class operator Explicit(const Value: Int64): BigCardinal;
    class operator Explicit(const Value: TBytes): BigCardinal;
    class operator Explicit(const Value: string): BigCardinal;

    class operator Equal(const A, B: BigCardinal): Boolean; inline;
    class operator NotEqual(const A, B: BigCardinal): Boolean; inline;
    class operator GreaterThan(const A, B: BigCardinal): Boolean; inline;
    class operator GreaterThanOrEqual(const A, B: BigCardinal): Boolean; inline;
    class operator LessThan(const A, B: BigCardinal): Boolean; inline;
    class operator LessThanOrEqual(const A, B: BigCardinal): Boolean; inline;

    class operator Add(const A, B: BigCardinal): BigCardinal;
    class operator Subtract(const A, B: BigCardinal): BigCardinal;
    class operator Multiply(const A, B: BigCardinal): BigCardinal;
    class operator IntDivide(const A, B: BigCardinal): BigCardinal;
    class operator Modulus(const A, B: BigCardinal): BigCardinal;

    class operator LeftShift(const A: BigCardinal; Shift: Cardinal): BigCardinal;
    class operator RightShift(const A: BigCardinal; Shift: Cardinal): BigCardinal;

    class operator BitwiseAnd(const A, B: BigCardinal): BigCardinal;
    class operator BitwiseOr(const A, B: BigCardinal): BigCardinal;

    function CompareToCard(const B: Cardinal): Integer;
    function CompareToInt(const B: Integer): Integer;
    function CompareTo(const B: Cardinal): Integer; overload; inline;
    function CompareTo(const B: Integer): Integer; overload; inline;

    class operator Equal(const A: BigCardinal; const B: Cardinal): Boolean; inline;
    class operator Equal(const A: Cardinal; const B: BigCardinal): Boolean; inline;
    class operator Equal(const A: BigCardinal; const B: Integer): Boolean; inline;
    class operator Equal(const A: Integer; const B: BigCardinal): Boolean; inline;
    class operator NotEqual(const A: BigCardinal; const B: Cardinal): Boolean; inline;
    class operator NotEqual(const A: Cardinal; const B: BigCardinal): Boolean; inline;
    class operator NotEqual(const A: BigCardinal; const B: Integer): Boolean; inline;
    class operator NotEqual(const A: Integer; const B: BigCardinal): Boolean; inline;
    class operator GreaterThan(const A: BigCardinal; const B: Cardinal): Boolean; inline;
    class operator GreaterThan(const A: Cardinal; const B: BigCardinal): Boolean; inline;
    class operator GreaterThan(const A: BigCardinal; const B: Integer): Boolean; inline;
    class operator GreaterThan(const A: Integer; const B: BigCardinal): Boolean; inline;
    class operator GreaterThanOrEqual(const A: BigCardinal; const B: Cardinal): Boolean; inline;
    class operator GreaterThanOrEqual(const A: Cardinal; const B: BigCardinal): Boolean; inline;
    class operator GreaterThanOrEqual(const A: BigCardinal; const B: Integer): Boolean; inline;
    class operator GreaterThanOrEqual(const A: Integer; const B: BigCardinal): Boolean; inline;
    class operator LessThan(const A: BigCardinal; const B: Cardinal): Boolean; inline;
    class operator LessThan(const A: Cardinal; const B: BigCardinal): Boolean; inline;
    class operator LessThan(const A: BigCardinal; const B: Integer): Boolean; inline;
    class operator LessThan(const A: Integer; const B: BigCardinal): Boolean; inline;
    class operator LessThanOrEqual(const A: BigCardinal; const B: Cardinal): Boolean; inline;
    class operator LessThanOrEqual(const A: Cardinal; const B: BigCardinal): Boolean; inline;
    class operator LessThanOrEqual(const A: BigCardinal; const B: Integer): Boolean; inline;
    class operator LessThanOrEqual(const A: Integer; const B: BigCardinal): Boolean; inline;

    function CompareToUInt64(const B: UInt64): Integer;
    function CompareToInt64(const B: Int64): Integer;
    function CompareTo(const B: UInt64): Integer; overload; inline;
    function CompareTo(const B: Int64): Integer; overload; inline;

    class operator Equal(const A: BigCardinal; const B: UInt64): Boolean; inline;
    class operator Equal(const A: UInt64; const B: BigCardinal): Boolean; inline;
    class operator Equal(const A: BigCardinal; const B: Int64): Boolean; inline;
    class operator Equal(const A: Int64; const B: BigCardinal): Boolean; inline;
    class operator NotEqual(const A: BigCardinal; const B: UInt64): Boolean; inline;
    class operator NotEqual(const A: UInt64; const B: BigCardinal): Boolean; inline;
    class operator NotEqual(const A: BigCardinal; const B: Int64): Boolean; inline;
    class operator NotEqual(const A: Int64; const B: BigCardinal): Boolean; inline;
    class operator GreaterThan(const A: BigCardinal; const B: UInt64): Boolean; inline;
    class operator GreaterThan(const A: UInt64; const B: BigCardinal): Boolean; inline;
    class operator GreaterThan(const A: BigCardinal; const B: Int64): Boolean; inline;
    class operator GreaterThan(const A: Int64; const B: BigCardinal): Boolean; inline;
    class operator GreaterThanOrEqual(const A: BigCardinal; const B: UInt64): Boolean; inline;
    class operator GreaterThanOrEqual(const A: UInt64; const B: BigCardinal): Boolean; inline;
    class operator GreaterThanOrEqual(const A: BigCardinal; const B: Int64): Boolean; inline;
    class operator GreaterThanOrEqual(const A: Int64; const B: BigCardinal): Boolean; inline;
    class operator LessThan(const A: BigCardinal; const B: UInt64): Boolean; inline;
    class operator LessThan(const A: UInt64; const B: BigCardinal): Boolean; inline;
    class operator LessThan(const A: BigCardinal; const B: Int64): Boolean; inline;
    class operator LessThan(const A: Int64; const B: BigCardinal): Boolean; inline;
    class operator LessThanOrEqual(const A: BigCardinal; const B: UInt64): Boolean; inline;
    class operator LessThanOrEqual(const A: UInt64; const B: BigCardinal): Boolean; inline;
    class operator LessThanOrEqual(const A: BigCardinal; const B: Int64): Boolean; inline;
    class operator LessThanOrEqual(const A: Int64; const B: BigCardinal): Boolean; inline;

    class function DivRem(const Dividend: BigCardinal; Divisor: Cardinal;
                          var Remainder: Cardinal): BigCardinal; overload; static;
    class function DivRem(const Dividend: Cardinal; Divisor: BigCardinal;
                          var Remainder: Cardinal): Cardinal; overload; static;

    class operator Add(const A: BigCardinal; const B: Cardinal): BigCardinal;
    class operator Add(const A: Cardinal; const B: BigCardinal): BigCardinal;
    class operator Subtract(const A: BigCardinal; const B: Cardinal): BigCardinal;
    class operator Subtract(const A: Cardinal; const B: BigCardinal): Cardinal;
    class operator Multiply(const A: BigCardinal; const B: Cardinal): BigCardinal;
    class operator Multiply(const A: Cardinal; const B: BigCardinal): BigCardinal;
    class operator IntDivide(const A: BigCardinal; const B: Cardinal): BigCardinal;
    class operator IntDivide(const A: Cardinal; const B: BigCardinal): Cardinal;
    class operator Modulus(const A: BigCardinal; const B: Cardinal): Cardinal;
    class operator Modulus(const A: Cardinal; const B: BigCardinal): Cardinal;
  end;

  BigInteger = record
  private
    FNumber: IBigNumber;
    function GetSign: Integer;
  public
    function ToString: string;
    function ToHexString(Digits: Integer = 0; const Prefix: string = '';
                         TwoCompl: Boolean = False): string;
    function ToBytes: TBytes;
    function TryParse(const S: string; TwoCompl: Boolean = False): Boolean;
    procedure Free;

    property Sign: Integer read GetSign;

    class function Abs(const A: BigInteger): BigInteger; static;
    class function Pow(const Base: BigInteger; Value: Cardinal): BigInteger; static;
    class function DivRem(const Dividend, Divisor: BigInteger;
                          var Remainder: BigInteger): BigInteger; overload; static;

    class function Sqrt(A: BigInteger): BigInteger; static;
    class function GCD(A, B: BigInteger): BigInteger; static;
    class function EGCD(A, B: BigInteger; var X, Y: BigInteger): BigInteger; static;
    class function ModPow(const BaseValue, ExpValue, Modulo: BigInteger): BigInteger; static;
    class function ModInverse(A, Modulo: BigInteger): BigInteger; static;

    class operator Implicit(const Value: BigCardinal): BigInteger; inline;
    class operator Explicit(const Value: BigInteger): BigCardinal; inline;

    class operator Explicit(const Value: BigInteger): Cardinal;
    class operator Explicit(const Value: BigInteger): UInt64;
    class operator Explicit(const Value: BigInteger): Integer;
    class operator Explicit(const Value: BigInteger): Int64;
    class operator Implicit(const Value: UInt32): BigInteger;
    class operator Implicit(const Value: UInt64): BigInteger;
    class operator Implicit(const Value: Int32): BigInteger;
    class operator Implicit(const Value: Int64): BigInteger;
    class operator Explicit(const Value: TBytes): BigInteger;
    class operator Explicit(const Value: string): BigInteger;

    class function Compare(const A, B: BigInteger): Integer; overload; static;
    class function Compare(const A: BigInteger; const B: BigCardinal): Integer; overload; static;
    class function Compare(const A: BigCardinal; const B: BigInteger): Integer; overload; static;
    function CompareTo(const B: BigInteger): Integer; overload; inline;
    function CompareTo(const B: BigCardinal): Integer; overload; inline;

    class operator Equal(const A, B: BigInteger): Boolean; inline;
    class operator Equal(const A: BigInteger; const B: BigCardinal): Boolean; inline;
    class operator Equal(const A: BigCardinal; const B: BigInteger): Boolean; inline;
    class operator NotEqual(const A, B: BigInteger): Boolean; inline;
    class operator NotEqual(const A: BigInteger; const B: BigCardinal): Boolean; inline;
    class operator NotEqual(const A: BigCardinal; const B: BigInteger): Boolean; inline;
    class operator GreaterThan(const A, B: BigInteger): Boolean; inline;
    class operator GreaterThan(const A: BigInteger; const B: BigCardinal): Boolean; inline;
    class operator GreaterThan(const A: BigCardinal; const B: BigInteger): Boolean; inline;
    class operator GreaterThanOrEqual(const A, B: BigInteger): Boolean; inline;
    class operator GreaterThanOrEqual(const A: BigInteger; const B: BigCardinal): Boolean; inline;
    class operator GreaterThanOrEqual(const A: BigCardinal; const B: BigInteger): Boolean; inline;
    class operator LessThan(const A, B: BigInteger): Boolean; inline;
    class operator LessThan(const A: BigInteger; const B: BigCardinal): Boolean; inline;
    class operator LessThan(const A: BigCardinal; const B: BigInteger): Boolean; inline;
    class operator LessThanOrEqual(const A, B: BigInteger): Boolean; inline;
    class operator LessThanOrEqual(const A: BigInteger; const B: BigCardinal): Boolean; inline;
    class operator LessThanOrEqual(const A: BigCardinal; const B: BigInteger): Boolean; inline;

    class operator Add(const A, B: BigInteger): BigInteger;
    class operator Subtract(const A, B: BigInteger): BigInteger;
    class operator Multiply(const A, B: BigInteger): BigInteger;
    class operator IntDivide(const A, B: BigInteger): BigInteger;
    class operator Modulus(const A, B: BigInteger): BigInteger;

    class operator LeftShift(const A: BigInteger; Shift: Cardinal): BigInteger;
    class operator RightShift(const A: BigInteger; Shift: Cardinal): BigInteger;

    class operator BitwiseAnd(const A, B: BigInteger): BigInteger;
    class operator BitwiseOr(const A, B: BigInteger): BigInteger;
    class operator BitwiseXor(const A, B: BigInteger): BigInteger;

    function CompareToCard(const B: Cardinal): Integer;
    function CompareToInt(const B: Integer): Integer;
    function CompareTo(const B: Cardinal): Integer; overload; inline;
    function CompareTo(const B: Integer): Integer; overload; inline;
    class operator Equal(const A: BigInteger; const B: Cardinal): Boolean; inline;
    class operator Equal(const A: Cardinal; const B: BigInteger): Boolean; inline;
    class operator Equal(const A: BigInteger; const B: Integer): Boolean; inline;
    class operator Equal(const A: Integer; const B: BigInteger): Boolean; inline;
    class operator NotEqual(const A: BigInteger; const B: Cardinal): Boolean; inline;
    class operator NotEqual(const A: Cardinal; const B: BigInteger): Boolean; inline;
    class operator NotEqual(const A: BigInteger; const B: Integer): Boolean; inline;
    class operator NotEqual(const A: Integer; const B: BigInteger): Boolean; inline;
    class operator GreaterThan(const A: BigInteger; const B: Cardinal): Boolean; inline;
    class operator GreaterThan(const A: Cardinal; const B: BigInteger): Boolean; inline;
    class operator GreaterThan(const A: BigInteger; const B: Integer): Boolean; inline;
    class operator GreaterThan(const A: Integer; const B: BigInteger): Boolean; inline;
    class operator GreaterThanOrEqual(const A: BigInteger; const B: Cardinal): Boolean; inline;
    class operator GreaterThanOrEqual(const A: Cardinal; const B: BigInteger): Boolean; inline;
    class operator GreaterThanOrEqual(const A: BigInteger; const B: Integer): Boolean; inline;
    class operator GreaterThanOrEqual(const A: Integer; const B: BigInteger): Boolean; inline;
    class operator LessThan(const A: BigInteger; const B: Cardinal): Boolean; inline;
    class operator LessThan(const A: Cardinal; const B: BigInteger): Boolean; inline;
    class operator LessThan(const A: BigInteger; const B: Integer): Boolean; inline;
    class operator LessThan(const A: Integer; const B: BigInteger): Boolean; inline;
    class operator LessThanOrEqual(const A: BigInteger; const B: Cardinal): Boolean; inline;
    class operator LessThanOrEqual(const A: Cardinal; const B: BigInteger): Boolean; inline;
    class operator LessThanOrEqual(const A: BigInteger; const B: Integer): Boolean; inline;
    class operator LessThanOrEqual(const A: Integer; const B: BigInteger): Boolean; inline;

    function CompareToDoubleUInt(const B: UInt64): Integer;
    function CompareToDoubleInt(const B: Int64): Integer;
    function CompareTo(const B: UInt64): Integer; overload; inline;
    function CompareTo(const B: Int64): Integer; overload; inline;

    class operator Equal(const A: BigInteger; const B: UInt64): Boolean; inline;
    class operator Equal(const A: UInt64; const B: BigInteger): Boolean; inline;
    class operator Equal(const A: BigInteger; const B: Int64): Boolean; inline;
    class operator Equal(const A: Int64; const B: BigInteger): Boolean; inline;
    class operator NotEqual(const A: BigInteger; const B: UInt64): Boolean; inline;
    class operator NotEqual(const A: UInt64; const B: BigInteger): Boolean; inline;
    class operator NotEqual(const A: BigInteger; const B: Int64): Boolean; inline;
    class operator NotEqual(const A: Int64; const B: BigInteger): Boolean; inline;
    class operator GreaterThan(const A: BigInteger; const B: UInt64): Boolean; inline;
    class operator GreaterThan(const A: UInt64; const B: BigInteger): Boolean; inline;
    class operator GreaterThan(const A: BigInteger; const B: Int64): Boolean; inline;
    class operator GreaterThan(const A: Int64; const B: BigInteger): Boolean; inline;
    class operator GreaterThanOrEqual(const A: BigInteger; const B: UInt64): Boolean; inline;
    class operator GreaterThanOrEqual(const A: UInt64; const B: BigInteger): Boolean; inline;
    class operator GreaterThanOrEqual(const A: BigInteger; const B: Int64): Boolean; inline;
    class operator GreaterThanOrEqual(const A: Int64; const B: BigInteger): Boolean; inline;
    class operator LessThan(const A: BigInteger; const B: UInt64): Boolean; inline;
    class operator LessThan(const A: UInt64; const B: BigInteger): Boolean; inline;
    class operator LessThan(const A: BigInteger; const B: Int64): Boolean; inline;
    class operator LessThan(const A: Int64; const B: BigInteger): Boolean; inline;
    class operator LessThanOrEqual(const A: BigInteger; const B: UInt64): Boolean; inline;
    class operator LessThanOrEqual(const A: UInt64; const B: BigInteger): Boolean; inline;
    class operator LessThanOrEqual(const A: BigInteger; const B: Int64): Boolean; inline;
    class operator LessThanOrEqual(const A: Int64; const B: BigInteger): Boolean; inline;

// arithmetic operations on BigInteger & Cardinal
    class operator Add(const A: BigInteger; const B: Cardinal): BigInteger;
    class operator Subtract(const A: BigInteger; const B: Cardinal): BigInteger;
    class operator Multiply(const A: BigInteger; const B: Cardinal): BigInteger;
    class operator IntDivide(const A: BigInteger; const B: Cardinal): BigInteger;
    class operator Modulus(const A: BigInteger; const B: Cardinal): BigInteger;
    class function DivRem(const Dividend: BigInteger; const Divisor: Cardinal;
                          var Remainder: BigInteger): BigInteger; overload; static;

// arithmetic operations on Cardinal & BigInteger
    class operator Add(const A: Cardinal; const B: BigInteger): BigInteger;
    class operator Subtract(const A: Cardinal; const B: BigInteger): BigInteger;
    class operator Multiply(const A: Cardinal; const B: BigInteger): BigInteger;
    class operator IntDivide(const A: Cardinal; const B: BigInteger): BigInteger;
    class operator Modulus(const A: Cardinal; const B: BigInteger): Cardinal;
    class function DivRem(const Dividend: Cardinal; const Divisor: BigInteger;
                          var Remainder: Cardinal): BigInteger; overload; static;

// arithmetic operations on BigInteger & Integer
    class operator Add(const A: BigInteger; const B: Integer): BigInteger;
    class operator Subtract(const A: BigInteger; const B: Integer): BigInteger;
    class operator Multiply(const A: BigInteger; const B: Integer): BigInteger;
    class operator IntDivide(const A: BigInteger; const B: Integer): BigInteger;
    class operator Modulus(const A: BigInteger; const B: Integer): Integer;
    class function DivRem(const Dividend: BigInteger; const Divisor: Integer;
                          var Remainder: Integer): BigInteger; overload; static;

// arithmetic operations on Integer & BigInteger
    class operator Add(const A: Integer; const B: BigInteger): BigInteger;
    class operator Subtract(const A: Integer; const B: BigInteger): BigInteger;
    class operator Multiply(const A: Integer; const B: BigInteger): BigInteger;
    class operator IntDivide(const A: Integer; const B: BigInteger): Integer;
    class operator Modulus(const A: Integer; const B: BigInteger): Integer;
    class function DivRem(const Dividend: Integer; const Divisor: BigInteger;
                          var Remainder: Integer): Integer; overload; static;
  end;

type
  EBigNumberError = class(Exception)
  private
    FCode: TF_RESULT;
  public
    constructor Create(ACode: TF_RESULT; const Msg: string = '');
    property Code: TF_RESULT read FCode;
  end;

procedure BigNumberError(ACode: TF_RESULT; const Msg: string = '');

implementation

const
  NumericsVersion = 55;

type
  TGetNumericsVersion = function(var Version: LongWord): TF_RESULT; stdcall;
  TBigNumberFromUInt32 = function(var A: IBigNumber; Value: Cardinal): TF_RESULT; stdcall;
  TBigNumberFromUInt64 = function(var A: IBigNumber; Value: UInt64): TF_RESULT; stdcall;
  TBigNumberFromInt32 = function(var A: IBigNumber; Value: Integer): TF_RESULT; stdcall;
  TBigNumberFromInt64 = function(var A: IBigNumber; Value: Int64): TF_RESULT; stdcall;
  TBigNumberFromPChar = function(var A: IBigNumber; P: PByte; L: Integer;
           CharSize: Integer; AllowNegative: Boolean; TwoCompl: Boolean): TF_RESULT; stdcall;
  TBigNumberFromPByte = function(var A: IBigNumber;
    P: PByte; L: Integer; AllowNegative: Boolean): TF_RESULT; stdcall;

var
  GetNumericsVersion: TGetNumericsVersion;
  BigNumberFromLimb: TBigNumberFromUInt32;
  BigNumberFromDblLimb: TBigNumberFromUInt64;
  BigNumberFromIntLimb: TBigNumberFromInt32;
  BigNumberFromDblIntLimb: TBigNumberFromInt64;
  BigNumberFromPChar: TBigNumberFromPChar;
  BigNumberFromPByte: TBigNumberFromPByte;

{ EBigNumberError }

constructor EBigNumberError.Create(ACode: TF_RESULT; const Msg: string);
begin
  if Msg = '' then
    inherited Create(Format('Big Number Error 0x%.8x', [ACode]))
  else
    inherited Create(Msg);
  FCode:= ACode;
end;

procedure BigNumberError(ACode: TF_RESULT; const Msg: string);
begin
  raise EBigNumberError.Create(ACode, Msg);
end;

procedure HResCheck(Value: TF_RESULT); inline;
begin
  if Value <> S_OK then
    BigNumberError(Value);
end;

{ BigCardinal }

function BigCardinal.ToString: string;
var
  BytesUsed: Integer;
  L: Integer;
  P, P1: PByte;
  I: Integer;

begin
  BytesUsed:= FNumber.GetSize;
// log(256) approximated from above by 41/17
  L:= (BytesUsed * 41) div 17 + 1;
  GetMem(P, L);
  try
    HResCheck(FNumber.ToDec(P, L));
    SetLength(Result, L);
    P1:= P;
    for I:= 1 to L do begin
      Result[I]:= Char(P1^);
      Inc(P1);
    end;
  finally
    FreeMem(P);
  end;
end;

function BigCardinal.ToHexString(Digits: Integer; const Prefix: string;
                         TwoCompl: Boolean): string;

var
  L: Integer;
  P, P1: PByte;
  HR: TF_RESULT;
  I: Integer;

begin
  Result:= '';
  HR:= FNumber.ToHex(nil, L, TwoCompl);
  if HR = TF_E_INVALIDARG then begin
    GetMem(P, L);
    try
      HResCheck(FNumber.ToHex(P, L, TwoCompl));
      if Digits < L then Digits:= L;
      Inc(Digits, Length(Prefix));
      SetLength(Result, Digits);
      Move(Pointer(Prefix)^, Pointer(Result)^, Length(Prefix) * SizeOf(Char));
      P1:= P;
      I:= Length(Prefix);
      while I + L < Digits do begin
        Inc(I);
        Result[I]:= '0';
      end;
      while I < Digits do begin
        Inc(I);
        Result[I]:= Char(P1^);
        Inc(P1);
      end;
    finally
      FreeMem(P);
    end;
  end
  else
    BigNumberError(HR);
end;

function BigCardinal.ToBytes: TBytes;
var
  HR: TF_RESULT;
  L: Cardinal;

begin
  L:= 0;
  HR:= FNumber.ToPByte(nil, L);
  if (HR = TF_E_INVALIDARG) and (L > 0) then begin
    SetLength(Result, L);
    HR:= FNumber.ToPByte(Pointer(Result), L);
  end;
  HResCheck(HR);
end;

function BigCardinal.TryParse(const S: string; TwoCompl: Boolean): Boolean;
begin
  Result:= BigNumberFromPChar(FNumber, Pointer(S), Length(S),
                              SizeOf(Char), False, TwoCompl) = TF_S_OK;
end;

procedure BigCardinal.Free;
begin
  FNumber:= nil;
end;

class function BigCardinal.Compare(const A, B: BigCardinal): Integer;
begin
  Result:= A.FNumber.CompareNumberU(B.FNumber);
end;

function BigCardinal.CompareTo(const B: BigCardinal): Integer;
begin
  Result:= Compare(Self, B);
end;

class function BigCardinal.Pow(const Base: BigCardinal; Value: Cardinal): BigCardinal;
begin
  HResCheck(Base.FNumber.PowU(Value, Result.FNumber));
end;

class function BigCardinal.DivRem(const Dividend, Divisor: BigCardinal;
                                  var Remainder: BigCardinal): BigCardinal;
begin
  HResCheck(Dividend.FNumber.DivRemNumberU(Divisor.FNumber,
            Result.FNumber, Remainder.FNumber));
end;

class operator BigCardinal.Explicit(const Value: BigCardinal): Cardinal;
begin
  HResCheck(Value.FNumber.ToLimb(Result));
end;

class operator BigCardinal.Explicit(const Value: BigCardinal): Integer;
begin
  HResCheck(Value.FNumber.ToIntLimb(Result));
end;

class operator BigCardinal.Explicit(const Value: BigCardinal): UInt64;
begin
  HResCheck(Value.FNumber.ToDblLimb(Result));
end;

class operator BigCardinal.Explicit(const Value: BigCardinal): Int64;
begin
  HResCheck(Value.FNumber.ToDblIntLimb(Result));
end;

class operator BigCardinal.Implicit(const Value: Cardinal): BigCardinal;
begin
  HResCheck(BigNumberFromLimb(Result.FNumber, Value));
end;

class operator BigCardinal.Implicit(const Value: UInt64): BigCardinal;
begin
  HResCheck(BigNumberFromDblLimb(Result.FNumber, Value));
end;

class operator BigCardinal.Explicit(const Value: Integer): BigCardinal;
begin
  if Value < 0 then
    BigNumberError(TF_E_INVALIDARG)
  else
    HResCheck(BigNumberFromIntLimb(Result.FNumber, Value));
end;

class operator BigCardinal.Explicit(const Value: Int64): BigCardinal;
begin
  if Value < 0 then
    BigNumberError(TF_E_INVALIDARG)
  else
    HResCheck(BigNumberFromDblIntLimb(Result.FNumber, Value));
end;

class operator BigCardinal.Explicit(const Value: TBytes): BigCardinal;
begin
  HResCheck(BigNumberFromPByte(Result.FNumber,
            Pointer(Value), Length(Value), False));
end;

class operator BigCardinal.Explicit(const Value: string): BigCardinal;
begin
  HResCheck(BigNumberFromPChar(Result.FNumber, Pointer(Value), Length(Value),
                               SizeOf(Char), False, False));
end;

class operator BigCardinal.Equal(const A, B: BigCardinal): Boolean;
begin
  Result:= Compare(A, B) = 0;
end;

class operator BigCardinal.NotEqual(const A, B: BigCardinal): Boolean;
begin
  Result:= Compare(A, B) <> 0;
end;

class operator BigCardinal.GreaterThan(const A, B: BigCardinal): Boolean;
begin
  Result:= Compare(A, B) > 0;
end;

class operator BigCardinal.GreaterThanOrEqual(const A, B: BigCardinal): Boolean;
begin
  Result:= Compare(A, B) >= 0;
end;

class operator BigCardinal.LessThan(const A, B: BigCardinal): Boolean;
begin
  Result:= Compare(A, B) < 0;
end;

class operator BigCardinal.LessThanOrEqual(const A, B: BigCardinal): Boolean;
begin
  Result:= Compare(A, B) <= 0;
end;

class operator BigCardinal.Add(const A, B: BigCardinal): BigCardinal;
begin
  HResCheck(A.FNumber.AddNumberU(B.FNumber, Result.FNumber));
end;

class operator BigCardinal.Subtract(const A, B: BigCardinal): BigCardinal;
begin
  HResCheck(A.FNumber.SubNumberU(B.FNumber, Result.FNumber));
end;

class operator BigCardinal.Multiply(const A, B: BigCardinal): BigCardinal;
begin
  HResCheck(A.FNumber.MulNumberU(B.FNumber, Result.FNumber));
end;

class operator BigCardinal.IntDivide(const A, B: BigCardinal): BigCardinal;
var
  Remainder: IBigNumber;

begin
  HResCheck(A.FNumber.DivRemNumberU(B.FNumber, Result.FNumber, Remainder));
end;

class operator BigCardinal.Modulus(const A, B: BigCardinal): BigCardinal;
var
  Quotient: IBigNumber;

begin
  HResCheck(A.FNumber.DivRemNumberU(B.FNumber, Quotient, Result.FNumber));
end;


class operator BigCardinal.LeftShift(const A: BigCardinal; Shift: Cardinal): BigCardinal;
begin
  HResCheck(A.FNumber.ShlNumber(Shift, Result.FNumber));
end;

class operator BigCardinal.RightShift(const A: BigCardinal; Shift: Cardinal): BigCardinal;
begin
  HResCheck(A.FNumber.ShrNumber(Shift, Result.FNumber));
end;

class operator BigCardinal.BitwiseAnd(const A, B: BigCardinal): BigCardinal;
begin
  HResCheck(A.FNumber.AndNumberU(B.FNumber, Result.FNumber));
end;

class operator BigCardinal.BitwiseOr(const A, B: BigCardinal): BigCardinal;
begin
  HResCheck(A.FNumber.OrNumberU(B.FNumber, Result.FNumber));
end;

function BigCardinal.CompareToCard(const B: Cardinal): Integer;
begin
  Result:= FNumber.CompareToLimbU(B);
end;

function BigCardinal.CompareToInt(const B: Integer): Integer;
begin
  Result:= FNumber.CompareToIntLimbU(B);
end;

function BigCardinal.CompareTo(const B: Cardinal): Integer;
begin
  Result:= CompareToCard(B);
end;

function BigCardinal.CompareTo(const B: Integer): Integer;
begin
  Result:= CompareToInt(B);
end;

class operator BigCardinal.Equal(const A: BigCardinal; const B: Cardinal): Boolean;
begin
  Result:= A.CompareToCard(B) = 0;
end;

class operator BigCardinal.Equal(const A: Cardinal; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToCard(A) = 0;
end;

class operator BigCardinal.Equal(const A: BigCardinal; const B: Integer): Boolean;
begin
  Result:= A.CompareToInt(B) = 0;
end;

class operator BigCardinal.Equal(const A: Integer; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToInt(A) = 0;
end;

class operator BigCardinal.NotEqual(const A: BigCardinal; const B: Cardinal): Boolean;
begin
  Result:= A.CompareToCard(B) <> 0;
end;

class operator BigCardinal.NotEqual(const A: Cardinal; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToCard(A) <> 0;
end;

class operator BigCardinal.NotEqual(const A: BigCardinal; const B: Integer): Boolean;
begin
  Result:= A.CompareToInt(B) <> 0;
end;

class operator BigCardinal.NotEqual(const A: Integer; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToInt(A) <> 0;
end;

class operator BigCardinal.GreaterThan(const A: BigCardinal; const B: Cardinal): Boolean;
begin
  Result:= A.CompareToCard(B) > 0;
end;

class operator BigCardinal.GreaterThan(const A: Cardinal; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToCard(A) < 0;
end;

class operator BigCardinal.GreaterThan(const A: BigCardinal; const B: Integer): Boolean;
begin
  Result:= A.CompareToInt(B) > 0;
end;

class operator BigCardinal.GreaterThan(const A: Integer; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToInt(A) < 0;
end;

class operator BigCardinal.GreaterThanOrEqual(const A: BigCardinal; const B: Cardinal): Boolean;
begin
  Result:= A.CompareToCard(B) >= 0;
end;

class operator BigCardinal.GreaterThanOrEqual(const A: Cardinal; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToCard(A) <= 0;
end;

class operator BigCardinal.GreaterThanOrEqual(const A: BigCardinal; const B: Integer): Boolean;
begin
  Result:= A.CompareToInt(B) >= 0;
end;

class operator BigCardinal.GreaterThanOrEqual(const A: Integer; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToInt(A) <= 0;
end;

class operator BigCardinal.LessThan(const A: BigCardinal; const B: Cardinal): Boolean;
begin
  Result:= A.CompareToCard(B) < 0;
end;

class operator BigCardinal.LessThan(const A: Cardinal; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToCard(A) > 0;
end;

class operator BigCardinal.LessThan(const A: BigCardinal; const B: Integer): Boolean;
begin
  Result:= A.CompareToInt(B) < 0;
end;

class operator BigCardinal.LessThan(const A: Integer; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToInt(A) > 0;
end;

class operator BigCardinal.LessThanOrEqual(const A: BigCardinal; const B: Cardinal): Boolean;
begin
  Result:= A.CompareToCard(B) <= 0;
end;

class operator BigCardinal.LessThanOrEqual(const A: Cardinal; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToCard(A) >= 0;
end;

class operator BigCardinal.LessThanOrEqual(const A: BigCardinal; const B: Integer): Boolean;
begin
  Result:= A.CompareToInt(B) <= 0;
end;

class operator BigCardinal.LessThanOrEqual(const A: Integer; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToInt(A) >= 0;
end;

{--- Comparison with 64-bit integers ---}

function BigCardinal.CompareToUInt64(const B: UInt64): Integer;
begin
  Result:= FNumber.CompareToDblLimbU(B);
end;

function BigCardinal.CompareToInt64(const B: Int64): Integer;
begin
  Result:= FNumber.CompareToDblIntLimbU(B);
end;

function BigCardinal.CompareTo(const B: UInt64): Integer;
begin
  Result:= CompareToUInt64(B);
end;

function BigCardinal.CompareTo(const B: Int64): Integer;
begin
  Result:= CompareToInt64(B);
end;

class operator BigCardinal.Equal(const A: BigCardinal; const B: UInt64): Boolean;
begin
  Result:= A.CompareToUInt64(B) = 0;
end;

class operator BigCardinal.Equal(const A: UInt64; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToUInt64(A) = 0;
end;

class operator BigCardinal.Equal(const A: BigCardinal; const B: Int64): Boolean;
begin
  Result:= A.CompareToInt64(B) = 0;
end;

class operator BigCardinal.Equal(const A: Int64; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToInt64(A) = 0;
end;

class operator BigCardinal.NotEqual(const A: BigCardinal; const B: UInt64): Boolean;
begin
  Result:= A.CompareToUInt64(B) <> 0;
end;

class operator BigCardinal.NotEqual(const A: UInt64; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToUInt64(A) <> 0;
end;

class operator BigCardinal.NotEqual(const A: BigCardinal; const B: Int64): Boolean;
begin
  Result:= A.CompareToInt64(B) <> 0;
end;

class operator BigCardinal.NotEqual(const A: Int64; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToInt64(A) <> 0;
end;

class operator BigCardinal.GreaterThan(const A: BigCardinal; const B: UInt64): Boolean;
begin
  Result:= A.CompareToUInt64(B) > 0;
end;

class operator BigCardinal.GreaterThan(const A: UInt64; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToUInt64(A) < 0;
end;

class operator BigCardinal.GreaterThan(const A: BigCardinal; const B: Int64): Boolean;
begin
  Result:= A.CompareToInt64(B) > 0;
end;

class operator BigCardinal.GreaterThan(const A: Int64; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToInt64(A) < 0;
end;

class operator BigCardinal.GreaterThanOrEqual(const A: BigCardinal; const B: UInt64): Boolean;
begin
  Result:= A.CompareToUInt64(B) >= 0;
end;

class operator BigCardinal.GreaterThanOrEqual(const A: UInt64; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToUInt64(A) <= 0;
end;

class operator BigCardinal.GreaterThanOrEqual(const A: BigCardinal; const B: Int64): Boolean;
begin
  Result:= A.CompareToInt64(B) >= 0;
end;

class operator BigCardinal.GreaterThanOrEqual(const A: Int64; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToInt64(A) <= 0;
end;

class operator BigCardinal.LessThan(const A: BigCardinal; const B: UInt64): Boolean;
begin
  Result:= A.CompareToUInt64(B) < 0;
end;

class operator BigCardinal.LessThan(const A: UInt64; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToUInt64(A) > 0;
end;

class operator BigCardinal.LessThan(const A: BigCardinal; const B: Int64): Boolean;
begin
  Result:= A.CompareToInt64(B) < 0;
end;

class operator BigCardinal.LessThan(const A: Int64; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToInt64(A) > 0;
end;

class operator BigCardinal.LessThanOrEqual(const A: BigCardinal; const B: UInt64): Boolean;
begin
  Result:= A.CompareToUInt64(B) <= 0;
end;

class operator BigCardinal.LessThanOrEqual(const A: UInt64; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToUInt64(A) >= 0;
end;

class operator BigCardinal.LessThanOrEqual(const A: BigCardinal; const B: Int64): Boolean;
begin
  Result:= A.CompareToInt64(B) <= 0;
end;

class operator BigCardinal.LessThanOrEqual(const A: Int64; const B: BigCardinal): Boolean;
begin
  Result:= B.CompareToInt64(A) >= 0;
end;



class operator BigCardinal.Add(const A: BigCardinal; const B: Cardinal): BigCardinal;
begin
  HResCheck(A.FNumber.AddLimbU(B, Result.FNumber));
end;

class operator BigCardinal.Add(const A: Cardinal; const B: BigCardinal): BigCardinal;
begin
  HResCheck(B.FNumber.AddLimbU(A, Result.FNumber));
end;

class operator BigCardinal.Subtract(const A: BigCardinal; const B: Cardinal): BigCardinal;
begin
  HResCheck(A.FNumber.SubLimbU(B, Result.FNumber));
end;

class operator BigCardinal.Subtract(const A: Cardinal; const B: BigCardinal): Cardinal;
begin
  HResCheck(B.FNumber.SubLimbU2(A, Result));
end;

class operator BigCardinal.Multiply(const A: BigCardinal; const B: Cardinal): BigCardinal;
begin
  HResCheck(A.FNumber.MulLimbU(B, Result.FNumber));
end;

class operator BigCardinal.Multiply(const A: Cardinal; const B: BigCardinal): BigCardinal;
begin
  HResCheck(B.FNumber.MulLimbU(A, Result.FNumber));
end;


class function BigCardinal.DivRem(const Dividend: BigCardinal;
               Divisor: Cardinal; var Remainder: Cardinal): BigCardinal;
begin
  HResCheck(Dividend.FNumber.DivRemLimbU(Divisor, Result.FNumber, Remainder));
end;

class function BigCardinal.DivRem(const Dividend: Cardinal;
               Divisor: BigCardinal; var Remainder: Cardinal): Cardinal;
begin
  HResCheck(Divisor.FNumber.DivRemLimbU2(Dividend, Result, Remainder));
end;

class operator BigCardinal.IntDivide(const A: BigCardinal; const B: Cardinal): BigCardinal;
var
  Remainder: Cardinal;

begin
  HResCheck(A.FNumber.DivRemLimbU(B, Result.FNumber, Remainder));
end;

class operator BigCardinal.IntDivide(const A: Cardinal; const B: BigCardinal): Cardinal;
var
  Remainder: Cardinal;

begin
  HResCheck(B.FNumber.DivRemLimbU2(A, Result, Remainder));
end;

class operator BigCardinal.Modulus(const A: BigCardinal; const B: Cardinal): Cardinal;
var
  Quotient: IBigNumber;

begin
  HResCheck(A.FNumber.DivRemLimbU(B, Quotient, Result));
end;


class operator BigCardinal.Modulus(const A: Cardinal; const B: BigCardinal): Cardinal;
var
  Quotient: Cardinal;

begin
  HResCheck(B.FNumber.DivRemLimbU2(A, Quotient, Result));
end;

{ -------------------------- BigInteger -------------------------- }

function BigInteger.GetSign: Integer;
begin
  Result:= FNumber.GetSign;
end;

function BigInteger.ToString: string;
var
  BytesUsed: Integer;
  L: Integer;
  P, P1: PByte;
  I: Integer;
  IsMinus: Boolean;

begin
  BytesUsed:= FNumber.GetSize;
// log(256) approximated from above by 41/17
  L:= (BytesUsed * 41) div 17 + 1;
  GetMem(P, L);
  try
    HResCheck(FNumber.ToDec(P, L));
    IsMinus:= GetSign < 0;
    if IsMinus then Inc(L);
    SetLength(Result, L);
    I:= 1;
    if IsMinus then begin
      Result[1]:= '-';
      Inc(I);
    end;
    P1:= P;
    while I <= L do begin
      Result[I]:= Char(P1^);
      Inc(P1);
      Inc(I);
    end;
  finally
    FreeMem(P);
  end;
end;

function BigInteger.ToHexString(Digits: Integer; const Prefix: string;
                                TwoCompl: Boolean): string;
const
  ASCII_8 = 56;   // Ord('8')

var
  L: Integer;
  P, P1: PByte;
  HR: TF_RESULT;
  Filler: Char;
  I: Integer;

begin
  Result:= '';
  HR:= FNumber.ToHex(nil, L, TwoCompl);
  if HR = TF_E_INVALIDARG then begin
    GetMem(P, L);
    try
      HResCheck(FNumber.ToHex(P, L, TwoCompl));
      if Digits < L then Digits:= L;
      I:= 1;
      if (FNumber.GetSign < 0) and not TwoCompl then begin
        Inc(I);
        SetLength(Result, Digits + Length(Prefix) + 1);
        Result[1]:= '-';
      end
      else
        SetLength(Result, Digits + Length(Prefix));
      Move(Pointer(Prefix)^, Result[I], Length(Prefix) * SizeOf(Char));
      Inc(I, Length(Prefix));
      if Digits > L then begin
        if TwoCompl and (P[L] >= ASCII_8) then Filler:= 'F'
        else Filler:= '0';
        while I + L <= Length(Result) do begin
          Result[I]:= Filler;
          Inc(I);
        end;
      end;
      P1:= P;
      while I <= Length(Result) do begin
        Result[I]:= Char(P1^);
        Inc(I);
        Inc(P1);
      end;
    finally
      FreeMem(P);
    end;
  end
  else
    BigNumberError(HR);
end;

function BigInteger.ToBytes: TBytes;
var
  HR: TF_RESULT;
  L: Cardinal;

begin
  Result:= nil;
  HR:= FNumber.ToPByte(nil, L);
  if (HR = TF_E_INVALIDARG) and (L > 0) then begin
    SetLength(Result, L);
    HR:= FNumber.ToPByte(Pointer(Result), L);
  end;
  HResCheck(HR);
end;

function BigInteger.TryParse(const S: string; TwoCompl: Boolean): Boolean;
begin
  Result:= BigNumberFromPChar(FNumber, Pointer(S), Length(S),
                              SizeOf(Char), True, TwoCompl) = TF_S_OK;
end;

procedure BigInteger.Free;
begin
  FNumber:= nil;
end;

class function BigInteger.Compare(const A, B: BigInteger): Integer;
begin
  Result:= A.FNumber.CompareNumber(B.FNumber);
end;

class function BigInteger.Compare(const A: BigInteger; const B: BigCardinal): Integer;
begin
  Result:= A.FNumber.CompareNumber(B.FNumber);
end;

class function BigInteger.Compare(const A: BigCardinal; const B: BigInteger): Integer;
begin
  Result:= A.FNumber.CompareNumber(B.FNumber);
end;

function BigInteger.CompareTo(const B: BigInteger): Integer;
begin
  Result:= Compare(Self, B);
end;

function BigInteger.CompareTo(const B: BigCardinal): Integer;
begin
  Result:= Compare(Self, B);
end;

class function BigInteger.Abs(const A: BigInteger): BigInteger;
begin
  HResCheck(A.FNumber.AbsNumber(Result.FNumber));
end;

class function BigInteger.Pow(const Base: BigInteger; Value: Cardinal): BigInteger;
begin
  HResCheck(Base.FNumber.Pow(Value, Result.FNumber));
end;

class function BigInteger.DivRem(const Dividend, Divisor: BigInteger;
               var Remainder: BigInteger): BigInteger;
begin
  HResCheck(Dividend.FNumber.DivRemNumber(Divisor.FNumber,
            Result.FNumber, Remainder.FNumber));
end;

class function BigInteger.Sqrt(A: BigInteger): BigInteger;
begin
  HResCheck(A.FNumber.SqrtNumber(Result.FNumber));
end;

class function BigInteger.GCD(A, B: BigInteger): BigInteger;
begin
  HResCheck(A.FNumber.GCD(B.FNumber, Result.FNumber));
end;

class function BigInteger.EGCD(A, B: BigInteger; var X, Y: BigInteger): BigInteger;
begin
  HResCheck(A.FNumber.EGCD(B.FNumber, Result.FNumber, X.FNumber, Y.FNumber));
end;

class function BigInteger.ModPow(const BaseValue, ExpValue,
               Modulo: BigInteger): BigInteger;
begin
  HResCheck(BaseValue.FNumber.ModPow(ExpValue.FNumber,
            Modulo.FNumber, Result.FNumber));
end;

class function BigInteger.ModInverse(A, Modulo: BigInteger): BigInteger;
begin
  HResCheck(A.FNumber.ModInverse(Modulo.FNumber, Result.FNumber));
end;

class operator BigInteger.Implicit(const Value: BigCardinal): BigInteger;
begin
  Result.FNumber:= Value.FNumber;
end;

class operator BigInteger.Explicit(const Value: BigInteger): BigCardinal;
begin
  if (Value.FNumber.GetSign < 0) then
    BigNumberError(TF_E_INVALIDARG);
  Result.FNumber:= Value.FNumber;
end;

class operator BigInteger.Explicit(const Value: BigInteger): Cardinal;
begin
  HResCheck(Value.FNumber.ToLimb(Result));
end;

class operator BigInteger.Explicit(const Value: BigInteger): UInt64;
begin
  HResCheck(Value.FNumber.ToDblLimb(Result));
end;

class operator BigInteger.Explicit(const Value: BigInteger): Integer;
begin
  HResCheck(Value.FNumber.ToIntLimb(Result));
end;

class operator BigInteger.Explicit(const Value: BigInteger): Int64;
begin
  HResCheck(Value.FNumber.ToDblIntLimb(Result));
end;

class operator BigInteger.Implicit(const Value: UInt32): BigInteger;
begin
  HResCheck(BigNumberFromLimb(Result.FNumber, Value));
end;

class operator BigInteger.Implicit(const Value: UInt64): BigInteger;
begin
  HResCheck(BigNumberFromDblLimb(Result.FNumber, Value));
end;

class operator BigInteger.Implicit(const Value: Int32): BigInteger;
begin
  HResCheck(BigNumberFromIntLimb(Result.FNumber, Value));
end;

class operator BigInteger.Implicit(const Value: Int64): BigInteger;
begin
  HResCheck(BigNumberFromDblIntLimb(Result.FNumber, Value));
end;

class operator BigInteger.Explicit(const Value: TBytes): BigInteger;
begin
  HResCheck(BigNumberFromPByte(Result.FNumber,
            Pointer(Value), Length(Value), True));
end;

class operator BigInteger.Explicit(const Value: string): BigInteger;
begin
  HResCheck(BigNumberFromPChar(Result.FNumber, Pointer(Value), Length(Value),
            SizeOf(Char), True, False));
end;

class operator BigInteger.Equal(const A, B: BigInteger): Boolean;
begin
  Result:= Compare(A, B) = 0;
end;

class operator BigInteger.Equal(const A: BigCardinal; const B: BigInteger): Boolean;
begin
  Result:= Compare(A, B) = 0;
end;

class operator BigInteger.Equal(const A: BigInteger; const B: BigCardinal): Boolean;
begin
  Result:= Compare(A, B) = 0;
end;

class operator BigInteger.NotEqual(const A, B: BigInteger): Boolean;
begin
  Result:= Compare(A, B) <> 0;
end;

class operator BigInteger.NotEqual(const A: BigCardinal; const B: BigInteger): Boolean;
begin
  Result:= Compare(A, B) <> 0;
end;

class operator BigInteger.NotEqual(const A: BigInteger; const B: BigCardinal): Boolean;
begin
  Result:= Compare(A, B) <> 0;
end;

class operator BigInteger.GreaterThan(const A, B: BigInteger): Boolean;
begin
  Result:= Compare(A, B) > 0;
end;

class operator BigInteger.GreaterThan(const A: BigInteger; const B: BigCardinal): Boolean;
begin
  Result:= Compare(A, B) > 0;
end;

class operator BigInteger.GreaterThan(const A: BigCardinal; const B: BigInteger): Boolean;
begin
  Result:= Compare(A, B) > 0;
end;

class operator BigInteger.GreaterThanOrEqual(const A, B: BigInteger): Boolean;
begin
  Result:= Compare(A, B) >= 0;
end;

class operator BigInteger.GreaterThanOrEqual(const A: BigInteger; const B: BigCardinal): Boolean;
begin
  Result:= Compare(A, B) >= 0;
end;

class operator BigInteger.GreaterThanOrEqual(const A: BigCardinal; const B: BigInteger): Boolean;
begin
  Result:= Compare(A, B) >= 0;
end;

class operator BigInteger.LessThan(const A, B: BigInteger): Boolean;
begin
  Result:= Compare(A, B) < 0;
end;

class operator BigInteger.LessThan(const A: BigInteger; const B: BigCardinal): Boolean;
begin
  Result:= Compare(A, B) < 0;
end;

class operator BigInteger.LessThan(const A: BigCardinal; const B: BigInteger): Boolean;
begin
  Result:= Compare(A, B) < 0;
end;

class operator BigInteger.LessThanOrEqual(const A, B: BigInteger): Boolean;
begin
  Result:= Compare(A, B) <= 0;
end;

class operator BigInteger.LessThanOrEqual(const A: BigInteger; const B: BigCardinal): Boolean;
begin
  Result:= Compare(A, B) <= 0;
end;

class operator BigInteger.LessThanOrEqual(const A: BigCardinal; const B: BigInteger): Boolean;
begin
  Result:= Compare(A, B) <= 0;
end;

class operator BigInteger.Add(const A, B: BigInteger): BigInteger;
begin
  HResCheck(A.FNumber.AddNumber(B.FNumber, Result.FNumber));
end;

class operator BigInteger.Subtract(const A, B: BigInteger): BigInteger;
begin
  HResCheck(A.FNumber.SubNumber(B.FNumber, Result.FNumber));
end;

class operator BigInteger.Multiply(const A, B: BigInteger): BigInteger;
begin
  HResCheck(A.FNumber.MulNumber(B.FNumber, Result.FNumber));
end;

class operator BigInteger.IntDivide(const A, B: BigInteger): BigInteger;
var
  Remainder: IBigNumber;

begin
  HResCheck(A.FNumber.DivRemNumber(B.FNumber, Result.FNumber, Remainder));
end;

class operator BigInteger.Modulus(const A, B: BigInteger): BigInteger;
var
  Quotient: IBigNumber;

begin
  HResCheck(A.FNumber.DivRemNumber(B.FNumber, Quotient, Result.FNumber));
end;

class operator BigInteger.LeftShift(const A: BigInteger; Shift: Cardinal): BigInteger;
begin
  HResCheck(A.FNumber.ShlNumber(Shift, Result.FNumber));
end;

class operator BigInteger.RightShift(const A: BigInteger; Shift: Cardinal): BigInteger;
begin
  HResCheck(A.FNumber.ShrNumber(Shift, Result.FNumber));
end;

class operator BigInteger.BitwiseAnd(const A, B: BigInteger): BigInteger;
begin
  HResCheck(A.FNumber.AndNumber(B.FNumber, Result.FNumber));
end;

class operator BigInteger.BitwiseOr(const A, B: BigInteger): BigInteger;
begin
  HResCheck(A.FNumber.OrNumber(B.FNumber, Result.FNumber));
end;

class operator BigInteger.BitwiseXor(const A, B: BigInteger): BigInteger;
begin
  HResCheck(A.FNumber.XorNumber(B.FNumber, Result.FNumber));
end;

function BigInteger.CompareToCard(const B: Cardinal): Integer;
begin
  Result:= FNumber.CompareToLimb(B);
end;

function BigInteger.CompareToInt(const B: Integer): Integer;
begin
  Result:= FNumber.CompareToIntLimb(B);
end;

function BigInteger.CompareTo(const B: Cardinal): Integer;
begin
  Result:= CompareToCard(B);
end;

function BigInteger.CompareTo(const B: Integer): Integer;
begin
  Result:= CompareToInt(B);
end;

class operator BigInteger.Equal(const A: BigInteger; const B: Cardinal): Boolean;
begin
  Result:= A.CompareToCard(B) = 0;
end;

class operator BigInteger.Equal(const A: Cardinal; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToCard(A) = 0;
end;

class operator BigInteger.Equal(const A: BigInteger; const B: Integer): Boolean;
begin
  Result:= A.CompareToInt(B) = 0;
end;

class operator BigInteger.Equal(const A: Integer; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToInt(A) = 0;
end;

class operator BigInteger.NotEqual(const A: BigInteger; const B: Cardinal): Boolean;
begin
  Result:= A.CompareToCard(B) <> 0;
end;

class operator BigInteger.NotEqual(const A: Cardinal; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToCard(A) <> 0;
end;

class operator BigInteger.NotEqual(const A: BigInteger; const B: Integer): Boolean;
begin
  Result:= A.CompareToInt(B) <> 0;
end;

class operator BigInteger.NotEqual(const A: Integer; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToInt(A) <> 0;
end;

class operator BigInteger.GreaterThan(const A: BigInteger; const B: Cardinal): Boolean;
begin
  Result:= A.CompareToCard(B) > 0;
end;

class operator BigInteger.GreaterThan(const A: Cardinal; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToCard(A) < 0;
end;

class operator BigInteger.GreaterThan(const A: BigInteger; const B: Integer): Boolean;
begin
  Result:= A.CompareToInt(B) > 0;
end;

class operator BigInteger.GreaterThan(const A: Integer; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToInt(A) < 0;
end;

class operator BigInteger.GreaterThanOrEqual(const A: BigInteger; const B: Cardinal): Boolean;
begin
  Result:= A.CompareToCard(B) >= 0;
end;

class operator BigInteger.GreaterThanOrEqual(const A: Cardinal; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToCard(A) <= 0;
end;

class operator BigInteger.GreaterThanOrEqual(const A: BigInteger; const B: Integer): Boolean;
begin
  Result:= A.CompareToInt(B) >= 0;
end;

class operator BigInteger.GreaterThanOrEqual(const A: Integer; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToInt(A) <= 0;
end;

class operator BigInteger.LessThan(const A: BigInteger; const B: Cardinal): Boolean;
begin
  Result:= A.CompareToCard(B) < 0;
end;

class operator BigInteger.LessThan(const A: Cardinal; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToCard(A) > 0;
end;

class operator BigInteger.LessThan(const A: BigInteger; const B: Integer): Boolean;
begin
  Result:= A.CompareToInt(B) < 0;
end;

class operator BigInteger.LessThan(const A: Integer; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToInt(A) > 0;
end;

class operator BigInteger.LessThanOrEqual(const A: BigInteger; const B: Cardinal): Boolean;
begin
  Result:= A.CompareToCard(B) <= 0;
end;

class operator BigInteger.LessThanOrEqual(const A: Cardinal; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToCard(A) >= 0;
end;

class operator BigInteger.LessThanOrEqual(const A: BigInteger; const B: Integer): Boolean;
begin
  Result:= A.CompareToInt(B) <= 0;
end;

class operator BigInteger.LessThanOrEqual(const A: Integer; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToInt(A) >= 0;
end;


function BigInteger.CompareToDoubleUInt(const B: UInt64): Integer;
begin
  Result:= FNumber.CompareToDblLimb(B);
end;

function BigInteger.CompareToDoubleInt(const B: Int64): Integer;
begin
  Result:= FNumber.CompareToDblIntLimb(B);
end;

function BigInteger.CompareTo(const B: UInt64): Integer;
begin
  Result:= CompareToDoubleUInt(B);
end;

function BigInteger.CompareTo(const B: Int64): Integer;
begin
  Result:= CompareToDoubleInt(B);
end;

class operator BigInteger.Equal(const A: BigInteger; const B: UInt64): Boolean;
begin
  Result:= A.CompareToDoubleUInt(B) = 0;
end;

class operator BigInteger.Equal(const A: UInt64; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToDoubleUInt(A) = 0;
end;

class operator BigInteger.Equal(const A: BigInteger; const B: Int64): Boolean;
begin
  Result:= A.CompareToDoubleInt(B) = 0;
end;

class operator BigInteger.Equal(const A: Int64; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToDoubleInt(A) = 0;
end;

class operator BigInteger.NotEqual(const A: BigInteger; const B: UInt64): Boolean;
begin
  Result:= A.CompareToDoubleUInt(B) <> 0;
end;

class operator BigInteger.NotEqual(const A: UInt64; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToDoubleUInt(A) <> 0;
end;

class operator BigInteger.NotEqual(const A: BigInteger; const B: Int64): Boolean;
begin
  Result:= A.CompareToDoubleInt(B) <> 0;
end;

class operator BigInteger.NotEqual(const A: Int64; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToDoubleInt(A) <> 0;
end;

class operator BigInteger.GreaterThan(const A: BigInteger; const B: UInt64): Boolean;
begin
  Result:= A.CompareToDoubleUInt(B) > 0;
end;

class operator BigInteger.GreaterThan(const A: UInt64; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToDoubleUInt(A) < 0;
end;

class operator BigInteger.GreaterThan(const A: BigInteger; const B: Int64): Boolean;
begin
  Result:= A.CompareToDoubleInt(B) > 0;
end;

class operator BigInteger.GreaterThan(const A: Int64; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToDoubleInt(A) < 0;
end;

class operator BigInteger.GreaterThanOrEqual(const A: BigInteger; const B: UInt64): Boolean;
begin
  Result:= A.CompareToDoubleUInt(B) >= 0;
end;

class operator BigInteger.GreaterThanOrEqual(const A: UInt64; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToDoubleUInt(A) <= 0;
end;

class operator BigInteger.GreaterThanOrEqual(const A: BigInteger; const B: Int64): Boolean;
begin
  Result:= A.CompareToDoubleInt(B) >= 0;
end;

class operator BigInteger.GreaterThanOrEqual(const A: Int64; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToDoubleInt(A) <= 0;
end;

class operator BigInteger.LessThan(const A: BigInteger; const B: UInt64): Boolean;
begin
  Result:= A.CompareToDoubleUInt(B) < 0;
end;

class operator BigInteger.LessThan(const A: UInt64; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToDoubleUInt(A) > 0;
end;

class operator BigInteger.LessThan(const A: BigInteger; const B: Int64): Boolean;
begin
  Result:= A.CompareToDoubleInt(B) < 0;
end;

class operator BigInteger.LessThan(const A: Int64; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToDoubleInt(A) > 0;
end;

class operator BigInteger.LessThanOrEqual(const A: BigInteger; const B: UInt64): Boolean;
begin
  Result:= A.CompareToDoubleUInt(B) <= 0;
end;

class operator BigInteger.LessThanOrEqual(const A: UInt64; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToDoubleUInt(A) >= 0;
end;

class operator BigInteger.LessThanOrEqual(const A: BigInteger; const B: Int64): Boolean;
begin
  Result:= A.CompareToDoubleInt(B) <= 0;
end;

class operator BigInteger.LessThanOrEqual(const A: Int64; const B: BigInteger): Boolean;
begin
  Result:= B.CompareToDoubleInt(A) >= 0;
end;

// -- arithmetic operations on BigInteger & Cardinal --

class operator BigInteger.Add(const A: BigInteger; const B: Cardinal): BigInteger;
begin
  HResCheck(A.FNumber.AddLimb(B, Result.FNumber));
end;

class operator BigInteger.Subtract(const A: BigInteger; const B: Cardinal): BigInteger;
begin
  HResCheck(A.FNumber.SubLimb(B, Result.FNumber));
end;

class operator BigInteger.Multiply(const A: BigInteger; const B: Cardinal): BigInteger;
begin
  HResCheck(A.FNumber.MulLimb(B, Result.FNumber));
end;

class operator BigInteger.IntDivide(const A: BigInteger; const B: Cardinal): BigInteger;
var
  Remainder: BigInteger;

begin
  HResCheck(A.FNumber.DivRemLimb(B, Result.FNumber, Remainder.FNumber));
end;

class operator BigInteger.Modulus(const A: BigInteger; const B: Cardinal): BigInteger;
var
  Quotient: BigInteger;

begin
  HResCheck(A.FNumber.DivRemLimb(B, Quotient.FNumber, Result.FNumber));
end;

class function BigInteger.DivRem(const Dividend: BigInteger;
               const Divisor: Cardinal; var Remainder: BigInteger): BigInteger;
begin
  HResCheck(Dividend.FNumber.DivRemLimb(Divisor, Result.FNumber, Remainder.FNumber));
end;

// -- arithmetic operations on Cardinal & BigInteger --

class operator BigInteger.Add(const A: Cardinal; const B: BigInteger): BigInteger;
begin
  HResCheck(B.FNumber.AddLimb(A, Result.FNumber));
end;

class operator BigInteger.Subtract(const A: Cardinal; const B: BigInteger): BigInteger;
begin
  HResCheck(B.FNumber.SubLimb2(A, Result.FNumber));
end;

class operator BigInteger.Multiply(const A: Cardinal; const B: BigInteger): BigInteger;
begin
  HResCheck(B.FNumber.MulLimb(A, Result.FNumber));
end;

class operator BigInteger.IntDivide(const A: Cardinal; const B: BigInteger): BigInteger;
var
  Remainder: Cardinal;

begin
  HResCheck(B.FNumber.DivRemLimb2(A, Result.FNumber, Remainder));
end;

class operator BigInteger.Modulus(const A: Cardinal; const B: BigInteger): Cardinal;
var
  Quotient: BigInteger;

begin
  HResCheck(B.FNumber.DivRemLimb2(A, Quotient.FNumber, Result));
end;

class function BigInteger.DivRem(const Dividend: Cardinal;
               const Divisor: BigInteger; var Remainder: Cardinal): BigInteger;
begin
  HResCheck(Divisor.FNumber.DivRemLimb2(Dividend, Result.FNumber, Remainder));
end;

// -- arithmetic operations on BigInteger & Integer --

class operator BigInteger.Add(const A: BigInteger; const B: Integer): BigInteger;
begin
  HResCheck(A.FNumber.AddIntLimb(B, Result.FNumber));
end;

class operator BigInteger.Subtract(const A: BigInteger; const B: Integer): BigInteger;
begin
  HResCheck(A.FNumber.SubIntLimb(B, Result.FNumber));
end;

class operator BigInteger.Multiply(const A: BigInteger; const B: Integer): BigInteger;
begin
  HResCheck(A.FNumber.MulIntLimb(B, Result.FNumber));
end;

class operator BigInteger.IntDivide(const A: BigInteger; const B: Integer): BigInteger;
var
  Remainder: Integer;

begin
  HResCheck(A.FNumber.DivRemIntLimb(B, Result.FNumber, Remainder));
end;

class operator BigInteger.Modulus(const A: BigInteger; const B: Integer): Integer;
var
  Quotient: BigInteger;

begin
  HResCheck(A.FNumber.DivRemIntLimb(B, Quotient.FNumber, Result));
end;

class function BigInteger.DivRem(const Dividend: BigInteger;
               const Divisor: Integer; var Remainder: Integer): BigInteger;
begin
  HResCheck(Dividend.FNumber.DivRemIntLimb(Divisor, Result.FNumber, Remainder));
end;

// -- arithmetic operations on Integer & BigInteger --

class operator BigInteger.Add(const A: Integer; const B: BigInteger): BigInteger;
begin
  HResCheck(B.FNumber.AddIntLimb(A, Result.FNumber));
end;

class operator BigInteger.Subtract(const A: Integer; const B: BigInteger): BigInteger;
begin
  HResCheck(B.FNumber.SubIntLimb2(A, Result.FNumber));
end;

class operator BigInteger.Multiply(const A: Integer; const B: BigInteger): BigInteger;
begin
  HResCheck(B.FNumber.MulIntLimb(A, Result.FNumber));
end;

class operator BigInteger.IntDivide(const A: Integer; const B: BigInteger): Integer;
var
  Remainder: Integer;

begin
  HResCheck(B.FNumber.DivRemIntLimb2(A, Result, Remainder));
end;

class operator BigInteger.Modulus(const A: Integer; const B: BigInteger): Integer;
var
  Quotient: Integer;

begin
  HResCheck(B.FNumber.DivRemIntLimb2(A, Quotient, Result));
end;

class function BigInteger.DivRem(const Dividend: Integer;
               const Divisor: BigInteger; var Remainder: Integer): Integer;
begin
  HResCheck(Divisor.FNumber.DivRemIntLimb2(Dividend, Result, Remainder));
end;


// ------------------------ DLL load stuff ---------------------------- //

const
{$IFDEF WIN64}
  LibName = 'numerics64.dll';
{$ELSE}
  LibName = 'numerics32.dll';
{$ENDIF}

function GetNumericsVersionError(var Version: LongWord): TF_RESULT; stdcall;
begin
  Result:= TF_E_LOADERROR;
end;

function BigNumberFrom32Error(var A: IBigNumber; Value: UInt32): TF_RESULT; stdcall;
begin
  Result:= TF_E_LOADERROR;
end;

function BigNumberFrom64Error(var A: IBigNumber; Value: UInt64): TF_RESULT; stdcall;
begin
  Result:= TF_E_LOADERROR;
end;

function BigNumberFromPCharError(var A: IBigNumber; P: PByte; L: Integer;
           CharSize: Integer; AllowNegative: Boolean; TwoCompl: Boolean): TF_RESULT; stdcall;
begin
  Result:= TF_E_LOADERROR;
end;

function BigNumberFromPByteError(var A: IBigNumber;
           P: PByte; L: Cardinal; AllowNegative: Boolean): TF_RESULT; stdcall;
begin
  Result:= TF_E_LOADERROR;
end;

var
  LibLoaded: Boolean = False;

function BigNumberFromLimbStub(var A: IBigNumber; Value: UInt32): TF_RESULT; stdcall;
begin
//  LoadNumerics(LibName);
  Result:= BigNumberFromLimb(A, Value);
end;

function BigNumberFromDblLimbStub(var A: IBigNumber; Value: UInt64): TF_RESULT; stdcall;
begin
//  LoadNumerics(LibName);
  Result:= BigNumberFromDblLimb(A, Value);
end;

function BigNumberFromIntLimbStub(var A: IBigNumber; Value: Int32): TF_RESULT; stdcall;
begin
//  LoadNumerics(LibName);
  Result:= BigNumberFromIntLimb(A, Value);
end;

function BigNumberFromDblIntLimbStub(var A: IBigNumber; Value: Int64): TF_RESULT; stdcall;
begin
//  LoadNumerics(LibName);
  Result:= BigNumberFromDblIntLimb(A, Value);
end;

function BigNumberFromPCharStub(var A: IBigNumber; P: PByte; L: Integer;
           CharSize: Integer; AllowNegative: Boolean; TwoCompl: Boolean): TF_RESULT; stdcall;
begin
//  LoadNumerics(LibName);
  Result:= BigNumberFromPCharStub(A, P, L, CharSize, AllowNegative, TwoCompl);
end;

function BigNumberFromPByteStub(var A: IBigNumber;
           P: PByte; L: Cardinal; AllowNegative: Boolean): TF_RESULT; stdcall;
begin
//  LoadNumerics(LibName);
  Result:= BigNumberFromPByteStub(A, P, L, AllowNegative);
end;

initialization
  @BigNumberFromLimb:= @BigNumberFromLimbStub;
  @BigNumberFromDblLimb:= @BigNumberFromDblLimbStub;
  @BigNumberFromIntLimb:= @BigNumberFromIntLimbStub;
  @BigNumberFromDblIntLimb:= @BigNumberFromDblIntLimbStub;
  @BigNumberFromPChar:= @BigNumberFromPCharStub;
  @BigNumberFromPByte:= @BigNumberFromPByteStub;

end.
