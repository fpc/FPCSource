{ C99 border cases of Math.Power, issue #29910 }
program tpowerborder;

{$mode objfpc}{$h+}

uses
  Math;

var
  lCode: Integer;

// Halt with the current code when aValue is not aExpected
procedure Check(aValue, aExpected: Float);

begin
  Inc(lCode);
  if IsNan(aExpected) then
    begin
    if not IsNan(aValue) then
      halt(lCode);
    end
  else if IsNan(aValue) or (aValue<>aExpected) then
    halt(lCode);
end;


var
  lPosInf, lNegInf, lNaN, lBig, lBigOdd: Float;

begin
  lCode:=0;
  lPosInf:=Infinity;
  lNegInf:=NegInfinity;

  { default exception mask, codes 1..6 }
  Check(Power(-1,lPosInf),1);
  Check(Power(-1,lNegInf),1);
  Check(Power(1,lPosInf),1);
  Check(Power(1,lNegInf),1);
  Check(Power(2,10),1024);
  Check(Power(-2,3),-8);

  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
  lNaN:=NaN;

  { codes 7..}
  Check(Power(1,lNaN),1);
  Check(Power(lNaN,0),1);
  Check(Power(lNaN,1),lNaN);
  Check(Power(2,lNaN),lNaN);
  Check(Power(lNaN,lPosInf),lNaN);

  Check(Power(0.5,lPosInf),0);
  Check(Power(-0.5,lPosInf),0);
  Check(Power(2,lPosInf),lPosInf);
  Check(Power(-2,lPosInf),lPosInf);
  Check(Power(0.5,lNegInf),lPosInf);
  Check(Power(-0.5,lNegInf),lPosInf);
  Check(Power(2,lNegInf),0);
  Check(Power(-2,lNegInf),0);
  Check(Power(0,lPosInf),0);
  Check(Power(0,lNegInf),lPosInf);

  lBig:=1e10;
  lBigOdd:=lBig+1;
  Check(Power(-2,lBig),lPosInf);
  Check(Power(-2,lBigOdd),lNegInf);
  Check(Power(-1,lBig),1);
  Check(Power(-1,lBigOdd),-1);
  Check(Power(-0.5,-lBig),lPosInf);
  Check(Power(-0.5,lBigOdd),0);
  writeln('ok');
end.
