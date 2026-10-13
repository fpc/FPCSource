{ ArcSecH near 1, for small arguments and outside its domain, all overloads, issue #41882 }
program tarsech;

{$mode objfpc}{$h+}

uses
  SysUtils, Math;

type
  TDoubleFunc = function(aX: Double): Double;


// Double overload of ArcSecH
function DoubleSecH(aX: Double): Double;

begin
  Result:=ArcSecH(aX);
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


// Returns true when aValue differs from aRef by at most aRel relative to aRef
function RelClose(aValue, aRef, aRel: Float): Boolean;

begin
  Result:=Abs(aValue-aRef)<=Abs(aRef)*aRel;
end;


// Checks that ArcSecH(aX) raises EInvalidOp or returns NaN
procedure CheckDomain(aCode: Integer; aX: Double);

var
  lValue: Double;
  lError: string;

begin
  lError:='';
  try
    lValue:=DoubleSecH(aX);
  except
    on E: Exception do
      lError:=E.ClassName;
  end;
  if lError='' then
    Check(aCode,IsNan(lValue),'ArcSecH('+FloatToStr(aX)+')='+FloatToStr(lValue)+', expected NaN or EInvalidOp')
  else
    Check(aCode,lError='EInvalidOp','ArcSecH('+FloatToStr(aX)+') raised '+lError+', expected EInvalidOp');
end;


var
  lSingle: Single;
  lDouble: Double;
{$ifdef FPC_HAS_TYPE_EXTENDED}
  lExt: Extended;
{$endif}

begin
  lSingle:=1;
  Check(10,ArcSecH(lSingle)=0,'Single ArcSecH(1) is 0');
  lSingle:=0;
  Check(11,ArcSecH(lSingle)=Infinity,'Single ArcSecH(0) is +Inf');
  lSingle:=1;
  lSingle:=lSingle-ldexp(lSingle,-24);
  Check(12,RelClose(ArcSecH(lSingle),3.4526699157604251908e-4,1e-6),'Single ArcSecH(1-2^-24)');
  lSingle:=1;
  lSingle:=lSingle-ldexp(lSingle,-12);
  Check(13,RelClose(ArcSecH(lSingle),2.2099335098031397862e-2,1e-6),'Single ArcSecH(1-2^-12)');
  lSingle:=0.5;
  Check(14,RelClose(ArcSecH(lSingle),1.3169578969248167086,1e-6),'Single ArcSecH(0.5)');

  lDouble:=1;
  Check(20,ArcSecH(lDouble)=0,'Double ArcSecH(1) is 0');
  lDouble:=0.9;
  Check(21,RelClose(ArcSecH(lDouble),4.6714530810326201813e-1,1e-15),'Double ArcSecH(0.9)');
  CheckDomain(22,-0.5);
  CheckDomain(23,-1.0);
  CheckDomain(24,1.5);
  CheckDomain(25,Infinity);

{$ifdef FPC_HAS_TYPE_EXTENDED}
  lExt:=1;
  Check(30,ArcSecH(lExt)=0,'Extended ArcSecH(1) is 0');
  lExt:=0;
  Check(31,ArcSecH(lExt)=Infinity,'Extended ArcSecH(0) is +Inf');
  lExt:=1;
  lExt:=lExt-ldexp(lExt,-64);
  Check(32,RelClose(ArcSecH(lExt),3.2927225399135962334e-10,1e-17),'Extended ArcSecH(1-2^-64)');
  lExt:=1;
  lExt:=lExt-ldexp(lExt,-40);
  Check(33,RelClose(ArcSecH(lExt),1.3486991523491201150e-6,1e-17),'Extended ArcSecH(1-2^-40)');
  lExt:=0.5;
  Check(34,RelClose(ArcSecH(lExt),1.3169578969248167086,1e-17),'Extended ArcSecH(0.5)');
  lExt:=1e-4000;
  Check(35,RelClose(ArcSecH(lExt),9211.0335191567426814,1e-17),'Extended ArcSecH(1e-4000)');
{$endif}
  writeln('ok');
end.
