{ ArTanH and LnXP1 for tiny, moderate, near-pole and special arguments, issue #41972 }
program tartanh;

{$mode objfpc}{$h+}

uses
  SysUtils, Math;

type
  TRefCase = record
    x: QWord;
    ref: QWord;
  end;
  TDoubleFunc = function(aX: Double): Double;

const
  MaxUlp = 4;

  { bit patterns of x and of the correctly rounded result }
  TanHCases: array[1..14] of TRefCase = (
    (x: QWord($01A56E1FC2F8F359); ref: QWord($01A56E1FC2F8F359)), { 1e-300 -> 1e-300 }
    (x: QWord($81A56E1FC2F8F359); ref: QWord($81A56E1FC2F8F359)), { -1e-300 -> -1e-300 }
    (x: QWord($3DDB7CDFD9D7BDBB); ref: QWord($3DDB7CDFD9D7BDBB)), { 1e-10 -> 1e-10 }
    (x: QWord($BDDB7CDFD9D7BDBB); ref: QWord($BDDB7CDFD9D7BDBB)), { -1e-10 -> -1e-10 }
    (x: QWord($3FD0000000000000); ref: QWord($3FD058AEFA811452)), { 0.25 -> 0.25541281188299536 }
    (x: QWord($BFD0000000000000); ref: QWord($BFD058AEFA811452)), { -0.25 -> -0.25541281188299536 }
    (x: QWord($3FE0000000000000); ref: QWord($3FE193EA7AAD030B)), { 0.5 -> 0.5493061443340549 }
    (x: QWord($3FE8000000000000); ref: QWord($3FEF2272AE325A57)), { 0.75 -> 0.9729550745276566 }
    (x: QWord($BFE8000000000000); ref: QWord($BFEF2272AE325A57)), { -0.75 -> -0.9729550745276566 }
    (x: QWord($3FECCCCCCCCCCCCD); ref: QWord($3FF78E360604B32D)), { 0.9 -> 1.4722194895832204 }
    (x: QWord($3FEFFFFFFF800000); ref: QWord($40257CD0E7006820)), { 1-2^-30 -> 10.743781298446322 }
    (x: QWord($BFEFFFFFFF800000); ref: QWord($C0257CD0E7006820)), { -(1-2^-30) -> -10.743781298446322 }
    (x: QWord($3FEFFFFFFFFFFFFF); ref: QWord($4032B708872320E2)), { 1-2^-53 -> 18.714973875118524 }
    (x: QWord($BFEFFFFFFFFFFFFF); ref: QWord($C032B708872320E2))  { -(1-2^-53) -> -18.714973875118524 }
  );
  XP1Cases: array[1..11] of TRefCase = (
    (x: QWord($01A56E1FC2F8F359); ref: QWord($01A56E1FC2F8F359)), { 1e-300 -> 1e-300 }
    (x: QWord($3DDB7CDFD9D7BDBB); ref: QWord($3DDB7CDFD9D1D693)), { 1e-10 -> 9.999999999500001e-11 }
    (x: QWord($BDDB7CDFD9D7BDBB); ref: QWord($BDDB7CDFD9DDA4E3)), { -1e-10 -> -1.00000000005e-10 }
    (x: QWord($3FE0000000000000); ref: QWord($3FD9F323ECBF984C)), { 0.5 -> 0.4054651081081644 }
    (x: QWord($BFE0000000000000); ref: QWord($BFE62E42FEFA39EF)), { -0.5 -> -0.6931471805599453 }
    (x: QWord($BCA0000000000000); ref: QWord($BCA0000000000000)), { -2^-53 -> -1.1102230246251565e-16 }
    (x: QWord($4008000000000000); ref: QWord($3FF62E42FEFA39EF)), { 3.0 -> 1.3862943611198906 }
    (x: QWord($4010000000000000); ref: QWord($3FF9C041F7ED8D33)), { 4.0 -> 1.6094379124341003 }
    (x: QWord($4202A05F20000000); ref: QWord($4037069E2AA3184E)), { 1e10 -> 23.025850930040455 }
    (x: QWord($7E37E43C8800759C); ref: QWord($4085963447F87FB5)), { 1e+300 -> 690.7755278982137 }
    (x: QWord($BFEFFFFFFFFFFFFF); ref: QWord($C0425E4F7B2737FA))  { -(1-2^-53) -> -36.7368005696771 }
  );


// Returns the double with bit pattern aBits
function BitsToDouble(aBits: QWord): Double;

begin
  Move(aBits,Result,SizeOf(Result));
end;


// Returns the bit pattern of aValue
function DoubleToBits(aValue: Double): QWord;

begin
  Move(aValue,Result,SizeOf(Result));
end;


// ArTanH rounded to Double
function DoubleTanH(aX: Double): Double;

begin
  Result:=ArTanH(aX);
end;


// ArcTanH rounded to Double
function DoubleArcTanH(aX: Double): Double;

begin
  Result:=ArcTanH(aX);
end;


// LnXP1 rounded to Double
function DoubleXP1(aX: Double): Double;

begin
  Result:=LnXP1(aX);
end;


// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg);
    halt(aCode);
    end;
end;


// Returns aFunc(aX), or NaN with aError set to the class name of a raised exception
function Call(aFunc: TDoubleFunc; aX: Double; out aError: string): Double;

begin
  aError:='';
  try
    Result:=aFunc(aX);
  except
    on E: Exception do
      begin
      aError:=E.ClassName;
      Result:=NaN;
      end;
  end;
end;


// Checks aFunc against every case of aCases within MaxUlp
procedure CheckTable(aCode: Integer; const aName: string; aFunc: TDoubleFunc; const aCases: array of TRefCase);

var
  i: Integer;
  lGot, lDiff: QWord;
  lX, lValue: Double;
  lError: string;

begin
  for i:=0 to High(aCases) do
    begin
    lX:=BitsToDouble(aCases[i].x);
    lValue:=Call(aFunc,lX,lError);
    Check(aCode,lError='',aName+'('+FloatToStr(lX)+') raised '+lError);
    lGot:=DoubleToBits(lValue);
    if lGot>aCases[i].ref then
      lDiff:=lGot-aCases[i].ref
    else
      lDiff:=aCases[i].ref-lGot;
    Check(aCode+1,lDiff<=MaxUlp,aName+'('+FloatToStr(lX)+')='+FloatToStr(lValue)+', expected '
      +FloatToStr(BitsToDouble(aCases[i].ref))+' ('+IntToStr(lDiff)+' ulp)');
    end;
end;


// Checks that aFunc(aX) returns aExpected without raising an exception, comparing bit patterns
procedure CheckSpecial(aCode: Integer; const aName: string; aFunc: TDoubleFunc; aX, aExpected: Double);

var
  lValue: Double;
  lError: string;

begin
  lValue:=Call(aFunc,aX,lError);
  Check(aCode,lError='',aName+'('+FloatToStr(aX)+') raised '+lError);
  Check(aCode,DoubleToBits(lValue)=DoubleToBits(aExpected),
    aName+'('+FloatToStr(aX)+')='+FloatToStr(lValue)+', expected '+FloatToStr(aExpected));
end;


// Checks that aFunc(aX) raises the exception class aExpected
procedure CheckRaises(aCode: Integer; const aName: string; aFunc: TDoubleFunc; aX: Double; const aExpected: string);

var
  lError: string;

begin
  Call(aFunc,aX,lError);
  Check(aCode,lError=aExpected,aName+'('+FloatToStr(aX)+') raised "'+lError+'", expected '+aExpected);
end;


var
  lZero, lNegZero, lValue: Double;
  lError: string;
  lMask: TFPUExceptionMask;

begin
  CheckTable(10,'ArTanH',@DoubleTanH,TanHCases);
  CheckTable(20,'LnXP1',@DoubleXP1,XP1Cases);

  lZero:=0;
  lNegZero:=-lZero;
  CheckSpecial(31,'ArTanH',@DoubleTanH,lZero,lZero);
  CheckSpecial(32,'ArTanH',@DoubleTanH,lNegZero,lNegZero);
  Check(33,IsNan(Call(@DoubleTanH,NaN,lError)) and (lError=''),'ArTanH(NaN) is NaN');
  Check(34,IsNan(Call(@DoubleArcTanH,NaN,lError)) and (lError=''),'ArcTanH(NaN) is NaN');
  Check(35,IsNan(Call(@DoubleXP1,NaN,lError)) and (lError=''),'LnXP1(NaN) is NaN');
  CheckSpecial(36,'LnXP1',@DoubleXP1,lZero,lZero);
  CheckSpecial(37,'LnXP1',@DoubleXP1,Infinity,Infinity);
  Check(38,ArcTanH(0.5)=ArTanH(0.5),'ArcTanH(0.5) equals ArTanH(0.5)');

  CheckRaises(41,'ArTanH',@DoubleTanH,1.0,'EZeroDivide');
  CheckRaises(42,'ArTanH',@DoubleTanH,-1.0,'EZeroDivide');
  CheckRaises(43,'ArTanH',@DoubleTanH,1.5,'EInvalidOp');
  CheckRaises(44,'ArTanH',@DoubleTanH,Infinity,'EInvalidOp');

  lMask:=GetExceptionMask;
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
  lValue:=ArTanH(1.0);
  Check(51,DoubleToBits(lValue)=DoubleToBits(Infinity),'masked ArTanH(1) is +Inf');
  lValue:=ArTanH(-1.0);
  Check(52,DoubleToBits(lValue)=DoubleToBits(NegInfinity),'masked ArTanH(-1) is -Inf');
  lValue:=ArTanH(1.5);
  Check(53,IsNan(lValue),'masked ArTanH(1.5) is NaN');
  lValue:=LnXP1(-1.0);
  Check(54,DoubleToBits(lValue)=DoubleToBits(NegInfinity),'masked LnXP1(-1) is -Inf');
  SetExceptionMask(lMask);
  writeln('ok');
end.
