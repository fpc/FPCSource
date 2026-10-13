{ ArSinH and ArCosH for huge, tiny and special arguments, issue #41881 }
program tarsinh;

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
  SinHCases: array[1..21] of TRefCase = (
    (x: QWord($601DD55745CBB7ED); ref: QWord($407659805ACD1225)), { 1e+155 -> 357.593836594637 }
    (x: QWord($E01DD55745CBB7ED); ref: QWord($C07659805ACD1225)), { -1e+155 -> -357.593836594637 }
    (x: QWord($7E37E43C8800759C); ref: QWord($40859BBFD8B83E44)), { 1e+300 -> 691.4686750787737 }
    (x: QWord($FE37E43C8800759C); ref: QWord($C0859BBFD8B83E44)), { -1e+300 -> -691.4686750787737 }
    (x: QWord($7FEFFFFFFFFFFFFF); ref: QWord($408633CE8FB9F87E)), { MaxDouble -> 710.475860073944 }
    (x: QWord($3DDB7CDFD9D7BDBB); ref: QWord($3DDB7CDFD9D7BDBB)), { 1e-10 -> 1e-10 }
    (x: QWord($BDDB7CDFD9D7BDBB); ref: QWord($BDDB7CDFD9D7BDBB)), { -1e-10 -> -1e-10 }
    (x: QWord($01A56E1FC2F8F359); ref: QWord($01A56E1FC2F8F359)), { 1e-300 -> 1e-300 }
    (x: QWord($0000000000000001); ref: QWord($0000000000000001)), { 5e-324 -> 5e-324 }
    (x: QWord($3FD0000000000000); ref: QWord($3FCFACFB2399E637)), { 0.25 -> 0.24746646154726346 }
    (x: QWord($BFD0000000000000); ref: QWord($BFCFACFB2399E637)), { -0.25 -> -0.24746646154726346 }
    (x: QWord($3FE0000000000000); ref: QWord($3FDECC2CAEC5160A)), { 0.5 -> 0.48121182505960347 }
    (x: QWord($3FE8000000000000); ref: QWord($3FE62E42FEFA39EF)), { 0.75 -> 0.6931471805599453 }
    (x: QWord($3FF0000000000000); ref: QWord($3FEC34366179D427)), { 1.0 -> 0.881373587019543 }
    (x: QWord($4000000000000000); ref: QWord($3FF719218313D087)), { 2.0 -> 1.4436354751788103 }
    (x: QWord($4024000000000000); ref: QWord($4007FC5C506D2BDB)), { 10.0 -> 2.99822295029797 }
    (x: QWord($416312D000000000); ref: QWord($4030CFAD9C367C43)), { 1e7 -> 16.811242831518268 }
    (x: QWord($4190000000000000); ref: QWord($4032B708872320E2)), { 2^26 -> 18.714973875118524 }
    (x: QWord($4190000004000000); ref: QWord($4032B708876320E2)), { 2^26+1 -> 18.714973890019685 }
    (x: QWord($4197D78400000000); ref: QWord($40331D23D3AD26E5)), { 1e8 -> 19.11382792451231 }
    (x: QWord($4415AF1D78B58C40); ref: QWord($40475F57369E9343))  { 1e+20 -> 46.74484904044086 }
  );
  CosHCases: array[1..11] of TRefCase = (
    (x: QWord($3FF0000000000000); ref: QWord($0000000000000000)), { 1.0 -> 0.0 }
    (x: QWord($3FF0000000000001); ref: QWord($3E56A09E667F3BCC)), { 1+2^-52 -> 2.1073424255447014e-08 }
    (x: QWord($3FF8000000000000); ref: QWord($3FEECC2CAEC5160A)), { 1.5 -> 0.9624236501192069 }
    (x: QWord($4000000000000000); ref: QWord($3FF5124271980435)), { 2.0 -> 1.3169578969248168 }
    (x: QWord($4024000000000000); ref: QWord($4007F21ED1CE05D4)), { 10.0 -> 2.993222846126381 }
    (x: QWord($416312D000000000); ref: QWord($4030CFAD9C367C42)), { 1e7 -> 16.811242831518264 }
    (x: QWord($4190000000000000); ref: QWord($4032B708872320E2)), { 2^26 -> 18.714973875118524 }
    (x: QWord($4197D78400000000); ref: QWord($40331D23D3AD26E5)), { 1e8 -> 19.11382792451231 }
    (x: QWord($601DD55745CBB7ED); ref: QWord($407659805ACD1225)), { 1e+155 -> 357.593836594637 }
    (x: QWord($7E37E43C8800759C); ref: QWord($40859BBFD8B83E44)), { 1e+300 -> 691.4686750787737 }
    (x: QWord($7FEFFFFFFFFFFFFF); ref: QWord($408633CE8FB9F87E))  { MaxDouble -> 710.475860073944 }
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


// ArSinH rounded to Double
function DoubleSinH(aX: Double): Double;

begin
  Result:=ArSinH(aX);
end;


// ArCosH rounded to Double
function DoubleCosH(aX: Double): Double;

begin
  Result:=ArCosH(aX);
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


var
  lZero, lNegZero: Double;
  lError: string;
{$ifdef FPC_HAS_TYPE_EXTENDED}
  lExt: Extended;
{$endif}

begin
  CheckTable(10,'ArSinH',@DoubleSinH,SinHCases);
  CheckTable(20,'ArCosH',@DoubleCosH,CosHCases);

  lZero:=0;
  lNegZero:=-lZero;
  CheckSpecial(31,'ArSinH',@DoubleSinH,lZero,lZero);
  CheckSpecial(32,'ArSinH',@DoubleSinH,lNegZero,lNegZero);
  CheckSpecial(33,'ArSinH',@DoubleSinH,Infinity,Infinity);
  CheckSpecial(34,'ArSinH',@DoubleSinH,NegInfinity,NegInfinity);
  Check(35,IsNan(Call(@DoubleSinH,NaN,lError)) and (lError=''),'ArSinH(NaN) is NaN');
  CheckSpecial(41,'ArCosH',@DoubleCosH,Infinity,Infinity);
  Check(42,IsNan(Call(@DoubleCosH,NaN,lError)) and (lError=''),'ArCosH(NaN) is NaN');
  Check(51,ArcSinH(-1e300)=ArSinH(-1e300),'ArcSinH(-1e300) equals ArSinH(-1e300)');
  Check(52,ArcCosH(1e300)=ArCosH(1e300),'ArcCosH(1e300) equals ArCosH(1e300)');
{$ifdef FPC_HAS_TYPE_EXTENDED}
  lExt:=1e4000;
  Check(61,SameValue(ArSinH(lExt),Extended(9211.0335191567426814),1e-12),'Extended ArSinH(1e4000)');
  Check(62,SameValue(ArSinH(-lExt),Extended(-9211.0335191567426814),1e-12),'Extended ArSinH(-1e4000)');
  Check(63,SameValue(ArCosH(lExt),Extended(9211.0335191567426814),1e-12),'Extended ArCosH(1e4000)');
  lExt:=1e-4000;
  Check(64,ArSinH(lExt)=lExt,'Extended ArSinH(1e-4000)');
{$endif}
  writeln('ok');
end.
