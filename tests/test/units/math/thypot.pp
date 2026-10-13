{ Hypot for finite, infinite and NaN arguments, issue #41884 }
program thypot;

{$mode objfpc}{$h+}

uses
  SysUtils, Math;

type
  THypotCase = record
    x: QWord;
    y: QWord;
    ref: QWord;
  end;

const
  MaxUlp = 2;

  { bit patterns of x, y and of the correctly rounded result }
  RefCases: array[1..17] of THypotCase = (
    (x: QWord($4008000000000000); y: QWord($4010000000000000); ref: QWord($4014000000000000)), { 3.0, 4.0 -> 5.0 }
    (x: QWord($C008000000000000); y: QWord($4010000000000000); ref: QWord($4014000000000000)), { -3.0, 4.0 -> 5.0 }
    (x: QWord($4008000000000000); y: QWord($C010000000000000); ref: QWord($4014000000000000)), { 3.0, -4.0 -> 5.0 }
    (x: QWord($4014000000000000); y: QWord($0000000000000000); ref: QWord($4014000000000000)), { 5.0, 0.0 -> 5.0 }
    (x: QWord($0000000000000000); y: QWord($C014000000000000); ref: QWord($4014000000000000)), { 0.0, -5.0 -> 5.0 }
    (x: QWord($7E37E43C8800759C); y: QWord($7E37E43C8800759C); ref: QWord($7E40E4D50F99B211)), { 1e+300, 1e+300 -> 1.4142135623730952e+300 }
    (x: QWord($FE37E43C8800759C); y: QWord($01A56E1FC2F8F359); ref: QWord($7E37E43C8800759C)), { -1e+300, 1e-300 -> 1e+300 }
    (x: QWord($7FE1CCF385EBC8A0); y: QWord($7FE1CCF385EBC8A0); ref: QWord($7FE92C80954C51F5)), { 1e+308, 1e+308 -> 1.4142135623730951e+308 }
    (x: QWord($01A56E1FC2F8F359); y: QWord($01A56E1FC2F8F359); ref: QWord($01AE4E8D12762225)), { 1e-300, 1e-300 -> 1.414213562373095e-300 }
    (x: QWord($0000000000000001); y: QWord($0000000000000001); ref: QWord($0000000000000001)), { 5e-324, 5e-324 -> 5e-324 }
    (x: QWord($00000000000007E8); y: QWord($00000000000017B8); ref: QWord($0000000000001900)), { 1e-320, 3e-320 -> 3.162e-320 }
    (x: QWord($3FF0000000000000); y: QWord($3DDB7CDFD9D7BDBB); ref: QWord($3FF0000000000000)), { 1.0, 1e-10 -> 1.0 }
    (x: QWord($3DDB7CDFD9D7BDBB); y: QWord($3FF0000000000000); ref: QWord($3FF0000000000000)), { 1e-10, 1.0 -> 1.0 }
    (x: QWord($3FF0000000000000); y: QWord($3FF0000000000000); ref: QWord($3FF6A09E667F3BCD)), { 1.0, 1.0 -> 1.4142135623730951 }
    (x: QWord($3FB999999999999A); y: QWord($3FC999999999999A); ref: QWord($3FCC9F25C5BFEDDA)), { 0.1, 0.2 -> 0.223606797749979 }
    (x: QWord($5FE7DDDF6B095FF1); y: QWord($5FE7DDDF6B095FF1); ref: QWord($5FF0E0551A9EDEA1)), { 1e+154, 1e+154 -> 1.414213562373095e+154 }
    (x: QWord($7FEFFFFFFFFFFFFF); y: QWord($3FF0000000000000); ref: QWord($7FEFFFFFFFFFFFFF))  { 1.7976931348623157e+308, 1.0 -> 1.7976931348623157e+308 }
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


// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg);
    halt(aCode);
    end;
end;


// Returns Hypot(aX,aY) rounded to Double, or NaN with aError set to the class name of a raised exception
function Call(aX, aY: Double; out aError: string): Double;

begin
  aError:='';
  try
    Result:=Hypot(aX,aY);
  except
    on E: Exception do
      begin
      aError:=E.ClassName;
      Result:=NaN;
      end;
  end;
end;


// Returns the arguments as text for messages
function ArgText(aX, aY: Double): string;

begin
  Result:='Hypot('+FloatToStr(aX)+','+FloatToStr(aY)+')';
end;


// Checks Hypot against every case of RefCases within MaxUlp
procedure CheckTable(aCode: Integer);

var
  i: Integer;
  lGot, lDiff: QWord;
  lX, lY, lValue: Double;
  lError: string;

begin
  for i:=Low(RefCases) to High(RefCases) do
    begin
    lX:=BitsToDouble(RefCases[i].x);
    lY:=BitsToDouble(RefCases[i].y);
    lValue:=Call(lX,lY,lError);
    Check(aCode,lError='',ArgText(lX,lY)+' raised '+lError);
    lGot:=DoubleToBits(lValue);
    if lGot>RefCases[i].ref then
      lDiff:=lGot-RefCases[i].ref
    else
      lDiff:=RefCases[i].ref-lGot;
    Check(aCode+1,lDiff<=MaxUlp,ArgText(lX,lY)+'='+FloatToStr(lValue)+', expected '
      +FloatToStr(BitsToDouble(RefCases[i].ref))+' ('+IntToStr(lDiff)+' ulp)');
    end;
end;


// Checks that Hypot(aX,aY) is +Inf without raising an exception
procedure CheckInf(aCode: Integer; aX, aY: Double);

var
  lValue: Double;
  lError: string;

begin
  lValue:=Call(aX,aY,lError);
  Check(aCode,lError='',ArgText(aX,aY)+' raised '+lError);
  Check(aCode,DoubleToBits(lValue)=DoubleToBits(Infinity),ArgText(aX,aY)+'='+FloatToStr(lValue)+', expected +Inf');
end;


// Checks that Hypot(aX,aY) is NaN without raising an exception
procedure CheckNan(aCode: Integer; aX, aY: Double);

var
  lValue: Double;
  lError: string;

begin
  lValue:=Call(aX,aY,lError);
  Check(aCode,lError='',ArgText(aX,aY)+' raised '+lError);
  Check(aCode,IsNan(lValue),ArgText(aX,aY)+'='+FloatToStr(lValue)+', expected NaN');
end;


// Checks the infinite, NaN and zero arguments; aBase offsets the exit codes
procedure CheckSpecial(aBase: Integer);

var
  lZero, lNegZero, lValue: Double;
  lError: string;

begin
  CheckInf(aBase+1,Infinity,Infinity);
  CheckInf(aBase+2,Infinity,NegInfinity);
  CheckInf(aBase+3,NegInfinity,NegInfinity);
  CheckInf(aBase+4,Infinity,NaN);
  CheckInf(aBase+5,NaN,NegInfinity);
  CheckInf(aBase+6,Infinity,5.0);
  CheckInf(aBase+7,-5.0,NegInfinity);
  CheckInf(aBase+8,0.0,Infinity);
  CheckNan(aBase+11,NaN,5.0);
  CheckNan(aBase+12,5.0,NaN);
  CheckNan(aBase+13,NaN,NaN);
  CheckNan(aBase+14,NaN,0.0);
  CheckNan(aBase+15,0.0,NaN);
  lZero:=0;
  lNegZero:=-lZero;
  lValue:=Call(lNegZero,lNegZero,lError);
  Check(aBase+21,(lError='') and (DoubleToBits(lValue)=0),'Hypot(-0,-0) is +0');
  lValue:=Call(lZero,lZero,lError);
  Check(aBase+22,(lError='') and (DoubleToBits(lValue)=0),'Hypot(0,0) is +0');
end;


var
  lMask: TFPUExceptionMask;
{$ifdef FPC_HAS_TYPE_EXTENDED}
  lExt: Extended;
{$endif}

begin
  CheckTable(10);
  CheckSpecial(20);
  lMask:=GetExceptionMask;
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
  CheckSpecial(50);
  SetExceptionMask(lMask);
{$ifdef FPC_HAS_TYPE_EXTENDED}
  lExt:=1e4000;
  Check(81,SameValue(Hypot(lExt,-lExt),Extended(1.4142135623730950488e4000),Extended(1e3982)),'Extended Hypot(1e4000,-1e4000)');
  lExt:=1e-4000;
  Check(82,SameValue(Hypot(lExt,lExt),Extended(1.4142135623730950488e-4000),Extended(1e-4018)),'Extended Hypot(1e-4000,1e-4000)');
{$endif}
  writeln('ok');
end.
