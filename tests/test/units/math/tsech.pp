{ SecH and CscH for 0, tiny, moderate, huge and special arguments, issue #41909 }
program tsech;

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
  SecHCases: array[1..14] of TRefCase = (
    (x: QWord($0000000000000000); ref: QWord($3FF0000000000000)), { 0.0 -> 1.0 }
    (x: QWord($3DDB7CDFD9D7BDBB); ref: QWord($3FF0000000000000)), { 1e-10 -> 1.0 }
    (x: QWord($BDDB7CDFD9D7BDBB); ref: QWord($3FF0000000000000)), { -1e-10 -> 1.0 }
    (x: QWord($3FE0000000000000); ref: QWord($3FEC60D1FF040DD0)), { 0.5 -> 0.8868188839700739 }
    (x: QWord($3FF0000000000000); ref: QWord($3FE4BCDC50ED6BE7)), { 1.0 -> 0.6480542736638853 }
    (x: QWord($BFF0000000000000); ref: QWord($3FE4BCDC50ED6BE7)), { -1.0 -> 0.6480542736638853 }
    (x: QWord($4000000000000000); ref: QWord($3FD102E75A02F642)), { 2.0 -> 0.26580222883407967 }
    (x: QWord($4034000000000000); ref: QWord($3E31B48655F37267)), { 20.0 -> 4.122307244877116e-09 }
    (x: QWord($C034000000000000); ref: QWord($3E31B48655F37267)), { -20.0 -> 4.122307244877116e-09 }
    (x: QWord($4085E00000000000); ref: QWord($00E14F2B0FB9307F)), { 700.0 -> 1.971935308751954e-304 }
    (x: QWord($4086280000000000); ref: QWord($00117FCABBC04671)), { 709.0 -> 2.4335615012468463e-308 }
    (x: QWord($C086280000000000); ref: QWord($00117FCABBC04671)), { -709.0 -> 2.4335615012468463e-308 }
    (x: QWord($408F400000000000); ref: QWord($0000000000000000)), { 1000.0 -> 0.0 }
    (x: QWord($C08F400000000000); ref: QWord($0000000000000000))  { -1000.0 -> 0.0 }
  );
  CscHCases: array[1..18] of TRefCase = (
    (x: QWord($01A56E1FC2F8F359); ref: QWord($7E37E43C8800759B)), { 1e-300 -> 9.999999999999999e+299 }
    (x: QWord($81A56E1FC2F8F359); ref: QWord($FE37E43C8800759B)), { -1e-300 -> -9.999999999999999e+299 }
    (x: QWord($3DDB7CDFD9D7BDBB); ref: QWord($4202A05F20000000)), { 1e-10 -> 10000000000.0 }
    (x: QWord($BDDB7CDFD9D7BDBB); ref: QWord($C202A05F20000000)), { -1e-10 -> -10000000000.0 }
    (x: QWord($3E45798EE2308C3A); ref: QWord($4197D78400000000)), { 1e-08 -> 100000000.0 }
    (x: QWord($3EE4F8B588E368F1); ref: QWord($40F869FFFFFE409B)), { 1e-05 -> 99999.99999833333 }
    (x: QWord($3FB999999999999A); ref: QWord($4023F77A033A0953)), { 0.1 -> 9.983352757296108 }
    (x: QWord($3FE0000000000000); ref: QWord($3FFEB45DC88DEFED)), { 0.5 -> 1.9190347513349437 }
    (x: QWord($3FE0000000000001); ref: QWord($3FFEB45DC88DEFEB)), { 0.5000000000000001 -> 1.9190347513349433 }
    (x: QWord($BFE0000000000001); ref: QWord($BFFEB45DC88DEFEB)), { -0.5000000000000001 -> -1.9190347513349433 }
    (x: QWord($3FF0000000000000); ref: QWord($3FEB3AB8A78B90C1)), { 1.0 -> 0.8509181282393216 }
    (x: QWord($BFF0000000000000); ref: QWord($BFEB3AB8A78B90C1)), { -1.0 -> -0.8509181282393216 }
    (x: QWord($4000000000000000); ref: QWord($3FD1A567DE21E2A6)), { 2.0 -> 0.2757205647717832 }
    (x: QWord($4034000000000000); ref: QWord($3E31B48655F37267)), { 20.0 -> 4.122307244877116e-09 }
    (x: QWord($4085E00000000000); ref: QWord($00E14F2B0FB9307F)), { 700.0 -> 1.971935308751954e-304 }
    (x: QWord($4086280000000000); ref: QWord($00117FCABBC04671)), { 709.0 -> 2.4335615012468463e-308 }
    (x: QWord($C086280000000000); ref: QWord($80117FCABBC04671)), { -709.0 -> -2.4335615012468463e-308 }
    (x: QWord($408F400000000000); ref: QWord($0000000000000000))  { 1000.0 -> 0.0 }
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


// Double overload of SecH
function DoubleSecH(aX: Double): Double;

begin
  Result:=SecH(aX);
end;


// Double overload of CscH
function DoubleCscH(aX: Double): Double;

begin
  Result:=CscH(aX);
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


// Returns true when aValue differs from aRef by at most aRel relative to aRef
function RelClose(aValue, aRef, aRel: Float): Boolean;

begin
  Result:=Abs(aValue-aRef)<=Abs(aRef)*aRel;
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


var
  lZero, lNegZero, lValue: Double;
  lError: string;
  lMask: TFPUExceptionMask;
  lSingle: Single;
{$ifdef FPC_HAS_TYPE_EXTENDED}
  lExt: Extended;
{$endif}

begin
  CheckTable(10,'SecH',@DoubleSecH,SecHCases);
  CheckTable(20,'CscH',@DoubleCscH,CscHCases);

  lZero:=0;
  lNegZero:=-lZero;
  CheckSpecial(31,'SecH',@DoubleSecH,Infinity,lZero);
  CheckSpecial(32,'SecH',@DoubleSecH,NegInfinity,lZero);
  Check(33,IsNan(Call(@DoubleSecH,NaN,lError)) and (lError=''),'SecH(NaN) is NaN');
  CheckSpecial(34,'CscH',@DoubleCscH,Infinity,lZero);
  CheckSpecial(35,'CscH',@DoubleCscH,NegInfinity,lNegZero);
  Check(36,IsNan(Call(@DoubleCscH,NaN,lError)) and (lError=''),'CscH(NaN) is NaN');
  Call(@DoubleCscH,lZero,lError);
  Check(37,lError='EZeroDivide','CscH(0) raised "'+lError+'", expected EZeroDivide');

  lMask:=GetExceptionMask;
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
  lValue:=CscH(lZero);
  Check(41,DoubleToBits(lValue)=DoubleToBits(Infinity),'masked CscH(0) is +Inf');
  lValue:=CscH(lNegZero);
  Check(42,DoubleToBits(lValue)=DoubleToBits(NegInfinity),'masked CscH(-0) is -Inf');
  lValue:=SecH(1000.0);
  Check(43,lValue=0,'masked SecH(1000) is 0');
  SetExceptionMask(lMask);

  lSingle:=50;
  Check(51,RelClose(SecH(lSingle),3.8574996959278355660e-22,1e-6),'Single SecH(50)');
  Check(52,RelClose(CscH(lSingle),3.8574996959278355660e-22,1e-6),'Single CscH(50)');
  lSingle:=1e-10;
  Check(53,RelClose(CscH(lSingle),1e10,1e-6),'Single CscH(1e-10)');
  lSingle:=1e-30;
  Check(54,RelClose(CscH(lSingle),1e30,1e-6),'Single CscH(1e-30)');
  lSingle:=-1000;
  Check(55,SecH(lSingle)=0,'Single SecH(-1000) is 0');
  Check(56,DoubleToBits(CscH(lSingle))=DoubleToBits(lNegZero),'Single CscH(-1000) is -0');

{$ifdef FPC_HAS_TYPE_EXTENDED}
  lExt:=1e-10;
  Check(61,RelClose(CscH(lExt),9.9999999999999999999833333e9,1e-17),'Extended CscH(1e-10)');
  lExt:=1e-3;
  Check(62,RelClose(CscH(lExt),9.9999983333335277777572751e2,1e-17),'Extended CscH(1e-3)');
  lExt:=11300;
  Check(63,RelClose(SecH(lExt),5.9345048474305935630653591e-4908,1e-15),'Extended SecH(11300)');
  lExt:=20000;
  Check(64,SecH(lExt)=0,'Extended SecH(20000) is 0');
  Check(65,CscH(lExt)=0,'Extended CscH(20000) is 0');
{$endif}
  writeln('ok');
end.
