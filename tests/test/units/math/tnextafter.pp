{ NextAfter, NextUp, NextDown and ULP for Single, Double and Extended, issue #39976 }
program tnextafter;

{$mode objfpc}{$h+}

uses
  SysUtils, Math;

{$ifdef linux}
  {$define HAS_LIBM}
  {$linklib m}
function c_nextafterf(x, y: Single): Single; cdecl; external name 'nextafterf';
function c_nextafter(x, y: Double): Double; cdecl; external name 'nextafter';
  {$ifdef FPC_HAS_TYPE_EXTENDED}
function c_nextafterl(x, y: Extended): Extended; cdecl; external name 'nextafterl';
  {$endif FPC_HAS_TYPE_EXTENDED}
{$endif linux}

{$ifdef HAS_LIBM}
var
  gOldMask: TFPUExceptionMask;


// Masks all floating-point exceptions; libm raises underflow for denormal results
procedure EnterLibm; inline;

begin
  gOldMask:=SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
end;


// Clears the flags set by libm and restores the exception mask
procedure LeaveLibm; inline;

begin
  ClearExceptions(False);
  SetExceptionMask(gOldMask);
end;


// nextafterf of the C library
function LibmNextAfter(x, y: Single): Single; overload;

begin
  EnterLibm;
  Result:=c_nextafterf(x,y);
  LeaveLibm;
end;


// nextafter of the C library
function LibmNextAfter(x, y: Double): Double; overload;

begin
  EnterLibm;
  Result:=c_nextafter(x,y);
  LeaveLibm;
end;


  {$ifdef FPC_HAS_TYPE_EXTENDED}
// nextafterl of the C library
function LibmNextAfter(x, y: Extended): Extended; overload;

begin
  EnterLibm;
  Result:=c_nextafterl(x,y);
  LeaveLibm;
end;
  {$endif FPC_HAS_TYPE_EXTENDED}
{$endif HAS_LIBM}

const
  cRandomCount = 200000;

type
  TSingleBits = packed record
    case Byte of
      0: (Value: Single);
      1: (Bits: DWord);
  end;

  TDoubleBits = packed record
    case Byte of
      0: (Value: Double);
      1: (Bits: QWord);
  end;

{$ifdef FPC_HAS_TYPE_EXTENDED}
  TExtendedBits = packed record
    case Byte of
      0: (Value: Extended);
      1: (Frac: QWord; Exp: Word);
  end;
{$endif FPC_HAS_TYPE_EXTENDED}

var
  gFailures: Integer;


// Counts a failure and shows aMsg; only the first 20 failures are shown
procedure Fail(const aMsg: string);

begin
  Inc(gFailures);
  if gFailures<=20 then
    writeln('Failed: ',aMsg);
end;


// Returns a random 32-bit pattern
function RandomDWord: DWord;

begin
  Result:=(DWord(Random($10000)) shl 16) or DWord(Random($10000));
end;


// Returns a random 64-bit pattern
function RandomQWord: QWord;

begin
  Result:=(QWord(RandomDWord) shl 32) or RandomDWord;
end;


{ ---------------------------------------------------------------------
  Single
  ---------------------------------------------------------------------}

const
  cSNeg = DWord($80000000);
  cSZero = DWord($00000000);
  cSMinDen = DWord($00000001);
  cSMaxDen = DWord($007FFFFF);
  cSMinNorm = DWord($00800000);
  cSOne = DWord($3F800000);
  cSTwo = DWord($40000000);
  cSMax = DWord($7F7FFFFF);
  cSInf = DWord($7F800000);
  cSNaN = DWord($7FC00000);


// Single with the bit pattern aBits
function S(aBits: DWord): Single;

var
  lRec: TSingleBits;

begin
  lRec.Bits:=aBits;
  Result:=lRec.Value;
end;


// Bit pattern of aValue
function SB(aValue: Single): DWord;

var
  lRec: TSingleBits;

begin
  lRec.Value:=aValue;
  Result:=lRec.Bits;
end;


// True when aBits is a NaN pattern
function SIsNaN(aBits: DWord): Boolean;

begin
  Result:=(aBits and $7FFFFFFF)>cSInf;
end;


// Fails unless aActual has the bit pattern aExpected, or both are NaN
procedure CheckS(const aWhat: string; aActual: Single; aExpected: DWord);

var
  lBits: DWord;

begin
  lBits:=SB(aActual);
  if SIsNaN(aExpected) and SIsNaN(lBits) then
    Exit;
  if lBits<>aExpected then
    Fail(Format('Single %s: got $%.8x, expected $%.8x',[aWhat,lBits,aExpected]));
end;


// Fails unless NextAfter(aX,aY) has the bit pattern aExpected
procedure CheckSAfter(aX, aY, aExpected: DWord);

begin
  CheckS(Format('NextAfter($%.8x,$%.8x)',[aX,aY]),NextAfter(S(aX),S(aY)),aExpected);
end;


// Expected ULP of the finite value with bit pattern aBits, from its exponent field
function SExpectedULP(aBits: DWord): DWord;

var
  lExp: DWord;

begin
  lExp:=(aBits shr 23) and $FF;
  if lExp=0 then
    Result:=1
  else if lExp>23 then
    Result:=(lExp-23) shl 23
  else
    Result:=DWord(1) shl (lExp-1);
end;


procedure TestSingleFixed;

begin
  CheckSAfter(cSOne,cSTwo,$3F800001);
  CheckSAfter(cSOne,cSZero,$3F7FFFFF);
  CheckSAfter(cSOne or cSNeg,cSTwo or cSNeg,$BF800001);
  CheckSAfter(cSOne or cSNeg,cSZero,$BF7FFFFF);
  CheckSAfter(cSZero,cSOne,cSMinDen);
  CheckSAfter(cSZero,cSOne or cSNeg,cSMinDen or cSNeg);
  CheckSAfter(cSNeg,cSOne,cSMinDen);
  CheckSAfter(cSNeg,cSOne or cSNeg,cSMinDen or cSNeg);
  CheckSAfter(cSZero,cSNeg,cSNeg);
  CheckSAfter(cSNeg,cSZero,cSZero);
  CheckSAfter(cSZero,cSZero,cSZero);
  CheckSAfter(cSOne,cSOne,cSOne);
  CheckSAfter(cSMinDen,cSZero,cSZero);
  CheckSAfter(cSMinDen or cSNeg,cSZero,cSNeg);
  CheckSAfter(cSMinDen,cSOne,2);
  CheckSAfter(cSMinDen or cSNeg,cSOne or cSNeg,cSNeg or 2);
  CheckSAfter(cSMaxDen,cSOne,cSMinNorm);
  CheckSAfter(cSMinNorm,cSZero,cSMaxDen);
  CheckSAfter(cSMinNorm or cSNeg,cSZero,cSMaxDen or cSNeg);
  CheckSAfter(cSMaxDen or cSNeg,cSInf or cSNeg,cSMinNorm or cSNeg);
  CheckSAfter(cSMax,cSInf,cSInf);
  CheckSAfter(cSMax or cSNeg,cSInf or cSNeg,cSInf or cSNeg);
  CheckSAfter(cSInf,cSZero,cSMax);
  CheckSAfter(cSInf or cSNeg,cSZero,cSMax or cSNeg);
  CheckSAfter(cSInf,cSInf,cSInf);
  CheckSAfter(cSInf or cSNeg,cSInf or cSNeg,cSInf or cSNeg);
  CheckSAfter(cSInf,cSInf or cSNeg,cSMax);
  CheckSAfter(cSInf or cSNeg,cSInf,cSMax or cSNeg);
  CheckSAfter(cSNaN,cSOne,cSNaN);
  CheckSAfter(cSOne,cSNaN,cSNaN);
  CheckSAfter(cSNaN,cSNaN,cSNaN);
  CheckSAfter(cSInf,cSNaN,cSNaN);
  if SB(NextAfter(S($7FC00123),S(cSOne)))<>$7FC00123 then
    Fail('Single NextAfter does not return the NaN argument unchanged');

  CheckS('NextUp(0)',NextUp(S(cSZero)),cSMinDen);
  CheckS('NextUp(-0)',NextUp(S(cSNeg)),cSMinDen);
  CheckS('NextDown(0)',NextDown(S(cSZero)),cSMinDen or cSNeg);
  CheckS('NextDown(-0)',NextDown(S(cSNeg)),cSMinDen or cSNeg);
  CheckS('NextUp(-MinDen)',NextUp(S(cSMinDen or cSNeg)),cSNeg);
  CheckS('NextDown(MinDen)',NextDown(S(cSMinDen)),cSZero);
  CheckS('NextUp(Max)',NextUp(S(cSMax)),cSInf);
  CheckS('NextDown(-Max)',NextDown(S(cSMax or cSNeg)),cSInf or cSNeg);
  CheckS('NextUp(Inf)',NextUp(S(cSInf)),cSInf);
  CheckS('NextDown(-Inf)',NextDown(S(cSInf or cSNeg)),cSInf or cSNeg);
  CheckS('NextUp(-Inf)',NextUp(S(cSInf or cSNeg)),cSMax or cSNeg);
  CheckS('NextDown(Inf)',NextDown(S(cSInf)),cSMax);
  CheckS('NextUp(1)',NextUp(S(cSOne)),$3F800001);
  CheckS('NextDown(1)',NextDown(S(cSOne)),$3F7FFFFF);
  CheckS('NextUp(NaN)',NextUp(S(cSNaN)),cSNaN);
  CheckS('NextDown(NaN)',NextDown(S(cSNaN)),cSNaN);

  CheckS('ULP(1)',ULP(S(cSOne)),$34000000);
  CheckS('ULP(-1)',ULP(S(cSOne or cSNeg)),$34000000);
  CheckS('ULP(2)',ULP(S(cSTwo)),$34800000);
  CheckS('ULP(NextDown(1))',ULP(S($3F7FFFFF)),$33800000);
  CheckS('ULP(0)',ULP(S(cSZero)),cSMinDen);
  CheckS('ULP(-0)',ULP(S(cSNeg)),cSMinDen);
  CheckS('ULP(MinDen)',ULP(S(cSMinDen)),cSMinDen);
  CheckS('ULP(MaxDen)',ULP(S(cSMaxDen)),cSMinDen);
  CheckS('ULP(MinNorm)',ULP(S(cSMinNorm)),cSMinDen);
  CheckS('ULP(Max)',ULP(S(cSMax)),$73800000);
  CheckS('ULP(-Max)',ULP(S(cSMax or cSNeg)),$73800000);
  CheckS('ULP(Inf)',ULP(S(cSInf)),cSInf);
  CheckS('ULP(-Inf)',ULP(S(cSInf or cSNeg)),cSInf);
  CheckS('ULP(NaN)',ULP(S(cSNaN)),cSNaN);
end;


procedure TestSingleRandom;

var
  i: Integer;
  lBits, lExpected: DWord;
  lX: Single;

begin
  for i:=1 to cRandomCount do
    begin
    lBits:=RandomDWord;
    if SIsNaN(lBits) then
      Continue;
    lX:=S(lBits);
    CheckS(Format('NextDown($%.8x)=-NextUp(-x)',[lBits]),NextDown(lX),SB(-NextUp(-lX)));
    if lBits<>cSInf then
      begin
      if not (NextUp(lX)>lX) then
        Fail(Format('Single NextUp($%.8x) not greater',[lBits]));
      lExpected:=lBits;
      if lExpected=cSNeg then
        lExpected:=cSZero;
      CheckS(Format('NextDown(NextUp($%.8x))',[lBits]),NextDown(NextUp(lX)),lExpected);
      end;
    if lBits<>(cSInf or cSNeg) then
      if not (NextDown(lX)<lX) then
        Fail(Format('Single NextDown($%.8x) not smaller',[lBits]));
    if (lBits and $7FFFFFFF)<cSInf then
      CheckS(Format('ULP($%.8x)',[lBits]),ULP(lX),SExpectedULP(lBits));
    end;
end;


{$ifdef HAS_LIBM}
procedure TestSingleLibm;

const
  cSpecial: array[0..13] of DWord = (cSZero,cSNeg,cSMinDen,cSMinDen or cSNeg,cSMaxDen,cSMinNorm or cSNeg,
    cSOne,cSOne or cSNeg,$3F800001,cSMax,cSMax or cSNeg,cSInf,cSInf or cSNeg,cSNaN);

var
  i, j: Integer;
  lX, lY: DWord;

begin
  for i:=Low(cSpecial) to High(cSpecial) do
    for j:=Low(cSpecial) to High(cSpecial) do
      CheckSAfter(cSpecial[i],cSpecial[j],SB(LibmNextAfter(S(cSpecial[i]),S(cSpecial[j]))));
  for i:=1 to cRandomCount do
    begin
    lX:=RandomDWord;
    if Odd(i) then
      lY:=RandomDWord
    else
      lY:=cSpecial[Random(Length(cSpecial))];
    // A signaling NaN raises when an x87 FPU loads it
    if SIsNaN(lX) then
      lX:=lX or $00400000;
    if SIsNaN(lY) then
      lY:=lY or $00400000;
    CheckSAfter(lX,lY,SB(LibmNextAfter(S(lX),S(lY))));
    end;
end;
{$endif HAS_LIBM}


{ ---------------------------------------------------------------------
  Double
  ---------------------------------------------------------------------}

const
  cDNeg = QWord($8000000000000000);
  cDZero = QWord($0000000000000000);
  cDMinDen = QWord($0000000000000001);
  cDMaxDen = QWord($000FFFFFFFFFFFFF);
  cDMinNorm = QWord($0010000000000000);
  cDOne = QWord($3FF0000000000000);
  cDTwo = QWord($4000000000000000);
  cDMax = QWord($7FEFFFFFFFFFFFFF);
  cDInf = QWord($7FF0000000000000);
  cDNaN = QWord($7FF8000000000000);


// Double with the bit pattern aBits
function D(aBits: QWord): Double;

var
  lRec: TDoubleBits;

begin
  lRec.Bits:=aBits;
  Result:=lRec.Value;
end;


// Bit pattern of aValue
function DB(aValue: Double): QWord;

var
  lRec: TDoubleBits;

begin
  lRec.Value:=aValue;
  Result:=lRec.Bits;
end;


// True when aBits is a NaN pattern
function DIsNaN(aBits: QWord): Boolean;

begin
  Result:=(aBits and QWord($7FFFFFFFFFFFFFFF))>cDInf;
end;


// Fails unless aActual has the bit pattern aExpected, or both are NaN
procedure CheckD(const aWhat: string; aActual: Double; aExpected: QWord);

var
  lBits: QWord;

begin
  lBits:=DB(aActual);
  if DIsNaN(aExpected) and DIsNaN(lBits) then
    Exit;
  if lBits<>aExpected then
    Fail(Format('Double %s: got $%.16x, expected $%.16x',[aWhat,lBits,aExpected]));
end;


// Fails unless NextAfter(aX,aY) has the bit pattern aExpected
procedure CheckDAfter(aX, aY, aExpected: QWord);

begin
  CheckD(Format('NextAfter($%.16x,$%.16x)',[aX,aY]),NextAfter(D(aX),D(aY)),aExpected);
end;


// Expected ULP of the finite value with bit pattern aBits, from its exponent field
function DExpectedULP(aBits: QWord): QWord;

var
  lExp: QWord;

begin
  lExp:=(aBits shr 52) and $7FF;
  if lExp=0 then
    Result:=1
  else if lExp>52 then
    Result:=(lExp-52) shl 52
  else
    Result:=QWord(1) shl (lExp-1);
end;


procedure TestDoubleFixed;

begin
  CheckDAfter(cDOne,cDTwo,$3FF0000000000001);
  CheckDAfter(cDOne,cDZero,$3FEFFFFFFFFFFFFF);
  CheckDAfter(cDOne or cDNeg,cDTwo or cDNeg,QWord($BFF0000000000001));
  CheckDAfter(cDOne or cDNeg,cDZero,QWord($BFEFFFFFFFFFFFFF));
  CheckDAfter(cDZero,cDOne,cDMinDen);
  CheckDAfter(cDZero,cDOne or cDNeg,cDMinDen or cDNeg);
  CheckDAfter(cDNeg,cDOne,cDMinDen);
  CheckDAfter(cDNeg,cDOne or cDNeg,cDMinDen or cDNeg);
  CheckDAfter(cDZero,cDNeg,cDNeg);
  CheckDAfter(cDNeg,cDZero,cDZero);
  CheckDAfter(cDZero,cDZero,cDZero);
  CheckDAfter(cDOne,cDOne,cDOne);
  CheckDAfter(cDMinDen,cDZero,cDZero);
  CheckDAfter(cDMinDen or cDNeg,cDZero,cDNeg);
  CheckDAfter(cDMinDen,cDOne,2);
  CheckDAfter(cDMinDen or cDNeg,cDOne or cDNeg,cDNeg or 2);
  CheckDAfter(cDMaxDen,cDOne,cDMinNorm);
  CheckDAfter(cDMinNorm,cDZero,cDMaxDen);
  CheckDAfter(cDMinNorm or cDNeg,cDZero,cDMaxDen or cDNeg);
  CheckDAfter(cDMaxDen or cDNeg,cDInf or cDNeg,cDMinNorm or cDNeg);
  CheckDAfter(cDMax,cDInf,cDInf);
  CheckDAfter(cDMax or cDNeg,cDInf or cDNeg,cDInf or cDNeg);
  CheckDAfter(cDInf,cDZero,cDMax);
  CheckDAfter(cDInf or cDNeg,cDZero,cDMax or cDNeg);
  CheckDAfter(cDInf,cDInf,cDInf);
  CheckDAfter(cDInf or cDNeg,cDInf or cDNeg,cDInf or cDNeg);
  CheckDAfter(cDInf,cDInf or cDNeg,cDMax);
  CheckDAfter(cDInf or cDNeg,cDInf,cDMax or cDNeg);
  CheckDAfter(cDNaN,cDOne,cDNaN);
  CheckDAfter(cDOne,cDNaN,cDNaN);
  CheckDAfter(cDNaN,cDNaN,cDNaN);
  CheckDAfter(cDInf,cDNaN,cDNaN);
  if DB(NextAfter(D($7FF8000000000123),D(cDOne)))<>$7FF8000000000123 then
    Fail('Double NextAfter does not return the NaN argument unchanged');

  CheckD('NextUp(0)',NextUp(D(cDZero)),cDMinDen);
  CheckD('NextUp(-0)',NextUp(D(cDNeg)),cDMinDen);
  CheckD('NextDown(0)',NextDown(D(cDZero)),cDMinDen or cDNeg);
  CheckD('NextDown(-0)',NextDown(D(cDNeg)),cDMinDen or cDNeg);
  CheckD('NextUp(-MinDen)',NextUp(D(cDMinDen or cDNeg)),cDNeg);
  CheckD('NextDown(MinDen)',NextDown(D(cDMinDen)),cDZero);
  CheckD('NextUp(Max)',NextUp(D(cDMax)),cDInf);
  CheckD('NextDown(-Max)',NextDown(D(cDMax or cDNeg)),cDInf or cDNeg);
  CheckD('NextUp(Inf)',NextUp(D(cDInf)),cDInf);
  CheckD('NextDown(-Inf)',NextDown(D(cDInf or cDNeg)),cDInf or cDNeg);
  CheckD('NextUp(-Inf)',NextUp(D(cDInf or cDNeg)),cDMax or cDNeg);
  CheckD('NextDown(Inf)',NextDown(D(cDInf)),cDMax);
  CheckD('NextUp(1)',NextUp(D(cDOne)),$3FF0000000000001);
  CheckD('NextDown(1)',NextDown(D(cDOne)),$3FEFFFFFFFFFFFFF);
  CheckD('NextUp(NaN)',NextUp(D(cDNaN)),cDNaN);
  CheckD('NextDown(NaN)',NextDown(D(cDNaN)),cDNaN);

  CheckD('ULP(1)',ULP(D(cDOne)),$3CB0000000000000);
  CheckD('ULP(-1)',ULP(D(cDOne or cDNeg)),$3CB0000000000000);
  CheckD('ULP(2)',ULP(D(cDTwo)),$3CC0000000000000);
  CheckD('ULP(NextDown(1))',ULP(D($3FEFFFFFFFFFFFFF)),$3CA0000000000000);
  CheckD('ULP(0)',ULP(D(cDZero)),cDMinDen);
  CheckD('ULP(-0)',ULP(D(cDNeg)),cDMinDen);
  CheckD('ULP(MinDen)',ULP(D(cDMinDen)),cDMinDen);
  CheckD('ULP(MaxDen)',ULP(D(cDMaxDen)),cDMinDen);
  CheckD('ULP(MinNorm)',ULP(D(cDMinNorm)),cDMinDen);
  CheckD('ULP(Max)',ULP(D(cDMax)),$7CA0000000000000);
  CheckD('ULP(-Max)',ULP(D(cDMax or cDNeg)),$7CA0000000000000);
  CheckD('ULP(Inf)',ULP(D(cDInf)),cDInf);
  CheckD('ULP(-Inf)',ULP(D(cDInf or cDNeg)),cDInf);
  CheckD('ULP(NaN)',ULP(D(cDNaN)),cDNaN);
end;


procedure TestDoubleRandom;

var
  i: Integer;
  lBits, lExpected: QWord;
  lX: Double;

begin
  for i:=1 to cRandomCount do
    begin
    lBits:=RandomQWord;
    if DIsNaN(lBits) then
      Continue;
    lX:=D(lBits);
    CheckD(Format('NextDown($%.16x)=-NextUp(-x)',[lBits]),NextDown(lX),DB(-NextUp(-lX)));
    if lBits<>cDInf then
      begin
      if not (NextUp(lX)>lX) then
        Fail(Format('Double NextUp($%.16x) not greater',[lBits]));
      lExpected:=lBits;
      if lExpected=cDNeg then
        lExpected:=cDZero;
      CheckD(Format('NextDown(NextUp($%.16x))',[lBits]),NextDown(NextUp(lX)),lExpected);
      end;
    if lBits<>(cDInf or cDNeg) then
      if not (NextDown(lX)<lX) then
        Fail(Format('Double NextDown($%.16x) not smaller',[lBits]));
    if (lBits and QWord($7FFFFFFFFFFFFFFF))<cDInf then
      CheckD(Format('ULP($%.16x)',[lBits]),ULP(lX),DExpectedULP(lBits));
    end;
end;


{$ifdef HAS_LIBM}
procedure TestDoubleLibm;

const
  cSpecial: array[0..13] of QWord = (cDZero,cDNeg,cDMinDen,cDMinDen or cDNeg,cDMaxDen,cDMinNorm or cDNeg,
    cDOne,cDOne or cDNeg,$3FF0000000000001,cDMax,cDMax or cDNeg,cDInf,cDInf or cDNeg,cDNaN);

var
  i, j: Integer;
  lX, lY: QWord;

begin
  for i:=Low(cSpecial) to High(cSpecial) do
    for j:=Low(cSpecial) to High(cSpecial) do
      CheckDAfter(cSpecial[i],cSpecial[j],DB(LibmNextAfter(D(cSpecial[i]),D(cSpecial[j]))));
  for i:=1 to cRandomCount do
    begin
    lX:=RandomQWord;
    if Odd(i) then
      lY:=RandomQWord
    else
      lY:=cSpecial[Random(Length(cSpecial))];
    // A signaling NaN raises when an x87 FPU loads it
    if DIsNaN(lX) then
      lX:=lX or QWord($0008000000000000);
    if DIsNaN(lY) then
      lY:=lY or QWord($0008000000000000);
    CheckDAfter(lX,lY,DB(LibmNextAfter(D(lX),D(lY))));
    end;
end;
{$endif HAS_LIBM}


{$ifdef FPC_HAS_TYPE_EXTENDED}
{ ---------------------------------------------------------------------
  Extended
  ---------------------------------------------------------------------}

const
  cENeg = Word($8000);
  cEInt = QWord($8000000000000000);
  cEAllOnes = QWord($FFFFFFFFFFFFFFFF);

type
  TExtPattern = record
    Exp: Word;
    Frac: QWord;
  end;


// Extended pattern with sign and exponent aExp and mantissa aFrac
function EP(aExp: Word; aFrac: QWord): TExtPattern;

begin
  Result.Exp:=aExp;
  Result.Frac:=aFrac;
end;


// Extended with the pattern aPattern
function E(const aPattern: TExtPattern): Extended;

var
  lRec: TExtendedBits;

begin
  lRec.Value:=0;
  lRec.Frac:=aPattern.Frac;
  lRec.Exp:=aPattern.Exp;
  Result:=lRec.Value;
end;


// Pattern of aValue
function EB(aValue: Extended): TExtPattern;

var
  lRec: TExtendedBits;

begin
  lRec.Value:=aValue;
  Result.Exp:=lRec.Exp;
  Result.Frac:=lRec.Frac;
end;


// aPattern with the sign bit set
function ENegate(const aPattern: TExtPattern): TExtPattern;

begin
  Result:=aPattern;
  Result.Exp:=Result.Exp or cENeg;
end;


// True when aPattern is a NaN
function EIsNaN(const aPattern: TExtPattern): Boolean;

begin
  Result:=((aPattern.Exp and $7FFF)=$7FFF) and ((aPattern.Frac and not cEInt)<>0);
end;


// Hexadecimal form of aPattern
function EHex(const aPattern: TExtPattern): string;

begin
  Result:=Format('$%.4x:%.16x',[aPattern.Exp,aPattern.Frac]);
end;


// Fails unless aActual has the pattern aExpected, or both are NaN
procedure CheckE(const aWhat: string; aActual: Extended; const aExpected: TExtPattern);

var
  lPattern: TExtPattern;

begin
  lPattern:=EB(aActual);
  if EIsNaN(aExpected) and EIsNaN(lPattern) then
    Exit;
  if (lPattern.Exp<>aExpected.Exp) or (lPattern.Frac<>aExpected.Frac) then
    Fail(Format('Extended %s: got %s, expected %s',[aWhat,EHex(lPattern),EHex(aExpected)]));
end;


// Fails unless NextAfter(aX,aY) has the pattern aExpected
procedure CheckEAfter(const aX, aY, aExpected: TExtPattern);

begin
  CheckE(Format('NextAfter(%s,%s)',[EHex(aX),EHex(aY)]),NextAfter(E(aX),E(aY)),aExpected);
end;


// Expected ULP of the finite value with pattern aPattern, from its exponent field
function EExpectedULP(const aPattern: TExtPattern): TExtPattern;

var
  lExp: Word;

begin
  lExp:=aPattern.Exp and $7FFF;
  if lExp=0 then
    Result:=EP(0,1)
  else if lExp>63 then
    Result:=EP(lExp-63,cEInt)
  else
    Result:=EP(0,QWord(1) shl (lExp-1));
end;


// Random finite or infinite pattern in canonical form
function RandomExtPattern: TExtPattern;

var
  lExp: Word;

begin
  lExp:=Random($8000);
  if lExp=$7FFF then
    Result:=EP($7FFF,cEInt)
  else if lExp=0 then
    Result:=EP(0,RandomQWord and not cEInt)
  else
    Result:=EP(lExp,RandomQWord or cEInt);
  if Odd(Random(2)) then
    Result:=ENegate(Result);
end;


procedure TestExtendedFixed;

var
  lZero, lNegZero, lMinDen, lMaxDen, lMinNorm, lOne, lTwo, lMax, lInf, lNaN: TExtPattern;

begin
  lZero:=EP(0,0);
  lNegZero:=EP(cENeg,0);
  lMinDen:=EP(0,1);
  lMaxDen:=EP(0,not cEInt);
  lMinNorm:=EP(1,cEInt);
  lOne:=EP($3FFF,cEInt);
  lTwo:=EP($4000,cEInt);
  lMax:=EP($7FFE,cEAllOnes);
  lInf:=EP($7FFF,cEInt);
  lNaN:=EP($7FFF,QWord($C000000000000000));

  CheckEAfter(lOne,lTwo,EP($3FFF,cEInt or 1));
  CheckEAfter(lOne,lZero,EP($3FFE,cEAllOnes));
  CheckEAfter(ENegate(lOne),ENegate(lTwo),ENegate(EP($3FFF,cEInt or 1)));
  CheckEAfter(ENegate(lOne),lZero,ENegate(EP($3FFE,cEAllOnes)));
  CheckEAfter(EP($3FFF,cEAllOnes),lTwo,lTwo);
  CheckEAfter(lTwo,lZero,EP($3FFF,cEAllOnes));
  CheckEAfter(lZero,lOne,lMinDen);
  CheckEAfter(lZero,ENegate(lOne),ENegate(lMinDen));
  CheckEAfter(lNegZero,lOne,lMinDen);
  CheckEAfter(lNegZero,ENegate(lOne),ENegate(lMinDen));
  CheckEAfter(lZero,lNegZero,lNegZero);
  CheckEAfter(lNegZero,lZero,lZero);
  CheckEAfter(lOne,lOne,lOne);
  CheckEAfter(lMinDen,lZero,lZero);
  CheckEAfter(ENegate(lMinDen),lZero,lNegZero);
  CheckEAfter(lMinDen,lOne,EP(0,2));
  CheckEAfter(lMaxDen,lOne,lMinNorm);
  CheckEAfter(lMinNorm,lZero,lMaxDen);
  CheckEAfter(ENegate(lMinNorm),lZero,ENegate(lMaxDen));
  CheckEAfter(ENegate(lMaxDen),ENegate(lInf),ENegate(lMinNorm));
  CheckEAfter(lMax,lInf,lInf);
  CheckEAfter(ENegate(lMax),ENegate(lInf),ENegate(lInf));
  CheckEAfter(lInf,lZero,lMax);
  CheckEAfter(ENegate(lInf),lZero,ENegate(lMax));
  CheckEAfter(lInf,lInf,lInf);
  CheckEAfter(lInf,ENegate(lInf),lMax);
  CheckEAfter(lNaN,lOne,lNaN);
  CheckEAfter(lOne,lNaN,lNaN);
  CheckEAfter(lInf,lNaN,lNaN);

  CheckE('NextUp(0)',NextUp(E(lZero)),lMinDen);
  CheckE('NextUp(-0)',NextUp(E(lNegZero)),lMinDen);
  CheckE('NextDown(0)',NextDown(E(lZero)),ENegate(lMinDen));
  CheckE('NextUp(-MinDen)',NextUp(E(ENegate(lMinDen))),lNegZero);
  CheckE('NextDown(MinDen)',NextDown(E(lMinDen)),lZero);
  CheckE('NextUp(Max)',NextUp(E(lMax)),lInf);
  CheckE('NextDown(-Max)',NextDown(E(ENegate(lMax))),ENegate(lInf));
  CheckE('NextUp(Inf)',NextUp(E(lInf)),lInf);
  CheckE('NextUp(-Inf)',NextUp(E(ENegate(lInf))),ENegate(lMax));
  CheckE('NextDown(Inf)',NextDown(E(lInf)),lMax);
  CheckE('NextUp(NaN)',NextUp(E(lNaN)),lNaN);

  CheckE('ULP(1)',ULP(E(lOne)),EP($3FFF-63,cEInt));
  CheckE('ULP(-1)',ULP(E(ENegate(lOne))),EP($3FFF-63,cEInt));
  CheckE('ULP(NextDown(1))',ULP(E(EP($3FFE,cEAllOnes))),EP($3FFE-63,cEInt));
  CheckE('ULP(0)',ULP(E(lZero)),lMinDen);
  CheckE('ULP(-0)',ULP(E(lNegZero)),lMinDen);
  CheckE('ULP(MaxDen)',ULP(E(lMaxDen)),lMinDen);
  CheckE('ULP(MinNorm)',ULP(E(lMinNorm)),lMinDen);
  CheckE('ULP(Max)',ULP(E(lMax)),EP($7FFE-63,cEInt));
  CheckE('ULP(Inf)',ULP(E(lInf)),lInf);
  CheckE('ULP(-Inf)',ULP(E(ENegate(lInf))),lInf);
  CheckE('ULP(NaN)',ULP(E(lNaN)),lNaN);
end;


procedure TestExtendedRandom;

var
  i: Integer;
  lPattern, lExpected: TExtPattern;
  lX: Extended;

begin
  for i:=1 to cRandomCount do
    begin
    lPattern:=RandomExtPattern;
    lX:=E(lPattern);
    CheckE(Format('NextDown(%s)=-NextUp(-x)',[EHex(lPattern)]),NextDown(lX),EB(-NextUp(-lX)));
    if not ((lPattern.Exp=$7FFF) and (lPattern.Frac=cEInt)) then
      begin
      if not (NextUp(lX)>lX) then
        Fail(Format('Extended NextUp(%s) not greater',[EHex(lPattern)]));
      lExpected:=lPattern;
      if (lExpected.Exp=cENeg) and (lExpected.Frac=0) then
        lExpected:=EP(0,0);
      CheckE(Format('NextDown(NextUp(%s))',[EHex(lPattern)]),NextDown(NextUp(lX)),lExpected);
      end;
    if not ((lPattern.Exp=($7FFF or cENeg)) and (lPattern.Frac=cEInt)) then
      if not (NextDown(lX)<lX) then
        Fail(Format('Extended NextDown(%s) not smaller',[EHex(lPattern)]));
    if (lPattern.Exp and $7FFF)<>$7FFF then
      CheckE(Format('ULP(%s)',[EHex(lPattern)]),ULP(lX),EExpectedULP(lPattern));
    end;
end;


{$ifdef HAS_LIBM}
procedure TestExtendedLibm;

var
  lSpecial: array[0..12] of TExtPattern;
  i, j: Integer;
  lX, lY: TExtPattern;

begin
  lSpecial[0]:=EP(0,0);
  lSpecial[1]:=EP(cENeg,0);
  lSpecial[2]:=EP(0,1);
  lSpecial[3]:=ENegate(EP(0,1));
  lSpecial[4]:=EP(0,not cEInt);
  lSpecial[5]:=ENegate(EP(1,cEInt));
  lSpecial[6]:=EP($3FFF,cEInt);
  lSpecial[7]:=ENegate(EP($3FFF,cEInt));
  lSpecial[8]:=EP($7FFE,cEAllOnes);
  lSpecial[9]:=ENegate(EP($7FFE,cEAllOnes));
  lSpecial[10]:=EP($7FFF,cEInt);
  lSpecial[11]:=ENegate(EP($7FFF,cEInt));
  lSpecial[12]:=EP($7FFF,QWord($C000000000000000));
  for i:=Low(lSpecial) to High(lSpecial) do
    for j:=Low(lSpecial) to High(lSpecial) do
      CheckEAfter(lSpecial[i],lSpecial[j],EB(LibmNextAfter(E(lSpecial[i]),E(lSpecial[j]))));
  for i:=1 to cRandomCount do
    begin
    lX:=RandomExtPattern;
    if Odd(i) then
      lY:=RandomExtPattern
    else
      lY:=lSpecial[Random(Length(lSpecial))];
    CheckEAfter(lX,lY,EB(LibmNextAfter(E(lX),E(lY))));
    end;
end;
{$endif HAS_LIBM}
{$endif FPC_HAS_TYPE_EXTENDED}


begin
  RandSeed:=39976;
  gFailures:=0;
  TestSingleFixed;
  TestSingleRandom;
  TestDoubleFixed;
  TestDoubleRandom;
{$ifdef FPC_HAS_TYPE_EXTENDED}
  TestExtendedFixed;
  TestExtendedRandom;
{$endif FPC_HAS_TYPE_EXTENDED}
{$ifdef HAS_LIBM}
  TestSingleLibm;
  TestDoubleLibm;
  {$ifdef FPC_HAS_TYPE_EXTENDED}
  TestExtendedLibm;
  {$endif FPC_HAS_TYPE_EXTENDED}
{$endif HAS_LIBM}
  if gFailures>0 then
    begin
    writeln(gFailures,' failures');
    halt(1);
    end;
  writeln('ok');
end.
