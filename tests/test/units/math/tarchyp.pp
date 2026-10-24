{ ArcCscH, ArcSecH and ArcCotH for 0, tiny, huge and negative arguments, issue #41880 }
program tarchyp;

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
  CscHCases: array[1..19] of TRefCase = (
    (x: QWord($3E112E0BE826D695); ref: QWord($40356A9A0B23D188)), { 1e-09 -> 21.416413017506358 }
    (x: QWord($BE112E0BE826D695); ref: QWord($C0356A9A0B23D188)), { -1e-09 -> -21.416413017506358 }
    (x: QWord($16687E92154EF7AC); ref: QWord($407CD35CD6CAD20F)), { 1e-200 -> 461.2101657793691 }
    (x: QWord($96687E92154EF7AC); ref: QWord($C07CD35CD6CAD20F)), { -1e-200 -> -461.2101657793691 }
    (x: QWord($3FF0000000000000); ref: QWord($3FEC34366179D427)), { 1.0 -> 0.881373587019543 }
    (x: QWord($BFF0000000000000); ref: QWord($BFEC34366179D427)), { -1.0 -> -0.881373587019543 }
    (x: QWord($7E37E43C8800759C); ref: QWord($01A56E1FC2F8F359)), { 1e+300 -> 1e-300 }
    (x: QWord($FE37E43C8800759C); ref: QWord($81A56E1FC2F8F359)), { -1e+300 -> -1e-300 }
    (x: QWord($3FE0000000000000); ref: QWord($3FF719218313D087)), { 0.5 -> 1.4436354751788103 }
    (x: QWord($BFE0000000000000); ref: QWord($BFF719218313D087)), { -0.5 -> -1.4436354751788103 }
    (x: QWord($4000000000000000); ref: QWord($3FDECC2CAEC5160A)), { 2.0 -> 0.48121182505960347 }
    (x: QWord($C000000000000000); ref: QWord($BFDECC2CAEC5160A)), { -2.0 -> -0.48121182505960347 }
    (x: QWord($39B4484BFEEBC2A0); ref: QWord($4051715325F7F438)), { 1e-30 -> 69.77069997038132 }
    (x: QWord($B9B4484BFEEBC2A0); ref: QWord($C051715325F7F438)), { -1e-30 -> -69.77069997038132 }
    (x: QWord($0000000000000001); ref: QWord($40874910D52D3052)), { 5e-324 -> 745.1332191019412 }
    (x: QWord($8000000000000001); ref: QWord($C0874910D52D3052)), { -5e-324 -> -745.1332191019412 }
    (x: QWord($4202A05F20000000); ref: QWord($3DDB7CDFD9D7BDBB)), { 1e10 -> 1e-10 }
    (x: QWord($C202A05F20000000); ref: QWord($BDDB7CDFD9D7BDBB)), { -1e10 -> -1e-10 }
    (x: QWord($3FE8000000000000); ref: QWord($3FF193EA7AAD030B))  { 0.75 -> 1.0986122886681098 }
  );
  SecHCases: array[1..9] of TRefCase = (
    (x: QWord($3E112E0BE826D695); ref: QWord($40356A9A0B23D188)), { 1e-09 -> 21.416413017506358 }
    (x: QWord($01A56E1FC2F8F359); ref: QWord($40859BBFD8B83E44)), { 1e-300 -> 691.4686750787737 }
    (x: QWord($0000000000000001); ref: QWord($40874910D52D3052)), { 5e-324 -> 745.1332191019412 }
    (x: QWord($3FE0000000000000); ref: QWord($3FF5124271980435)), { 0.5 -> 1.3169578969248168 }
    (x: QWord($3FD0000000000000); ref: QWord($400081EB4B421591)), { 0.25 -> 2.0634370688955603 }
    (x: QWord($3FECCCCCCCCCCCCD); ref: QWord($3FDDE5B56F322278)), { 0.9 -> 0.46714530810326194 }
    (x: QWord($3FEFFFFFFF800000); ref: QWord($3F06A09E66A4F22A)), { 1-2^-30 -> 4.315837289190314e-05 }
    (x: QWord($3FEFFFFFFFFFFFFE); ref: QWord($3E56A09E667F3BCD)), { 1-2^-52 -> 2.1073424255447017e-08 }
    (x: QWord($3FF0000000000000); ref: QWord($0000000000000000))  { 1.0 -> 0.0 }
  );
  CotHCases: array[1..10] of TRefCase = (
    (x: QWord($3FF8000000000000); ref: QWord($3FE9C041F7ED8D33)), { 1.5 -> 0.8047189562170501 }
    (x: QWord($BFF8000000000000); ref: QWord($BFE9C041F7ED8D33)), { -1.5 -> -0.8047189562170501 }
    (x: QWord($4202A05F20000000); ref: QWord($3DDB7CDFD9D7BDBB)), { 1e10 -> 1e-10 }
    (x: QWord($C202A05F20000000); ref: QWord($BDDB7CDFD9D7BDBB)), { -1e10 -> -1e-10 }
    (x: QWord($4415AF1D78B58C40); ref: QWord($3BC79CA10C924223)), { 1e+20 -> 1e-20 }
    (x: QWord($C415AF1D78B58C40); ref: QWord($BBC79CA10C924223)), { -1e+20 -> -1e-20 }
    (x: QWord($7E37E43C8800759C); ref: QWord($01A56E1FC2F8F359)), { 1e+300 -> 1e-300 }
    (x: QWord($3FF0000000400000); ref: QWord($40257CD0E7046820)), { 1+2^-30 -> 10.743781298911983 }
    (x: QWord($BFF0000000400000); ref: QWord($C0257CD0E7046820)), { -(1+2^-30) -> -10.743781298911983 }
    (x: QWord($4008000000000000); ref: QWord($3FD62E42FEFA39EF))  { 3.0 -> 0.34657359027997264 }
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


// Double overload of ArcCscH
function DoubleCscH(aX: Double): Double;

begin
  Result:=ArcCscH(aX);
end;


// Double overload of ArcSecH
function DoubleSecH(aX: Double): Double;

begin
  Result:=ArcSecH(aX);
end;


// Double overload of ArcCotH
function DoubleCotH(aX: Double): Double;

begin
  Result:=ArcCotH(aX);
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
  lSingle: Single;
  lZero, lNegZero: Double;
  lError: string;
{$ifdef FPC_HAS_TYPE_EXTENDED}
  lExt: Extended;
{$endif}

begin
  CheckTable(10,'ArcCscH',@DoubleCscH,CscHCases);
  CheckTable(20,'ArcSecH',@DoubleSecH,SecHCases);
  CheckTable(30,'ArcCotH',@DoubleCotH,CotHCases);

  lZero:=0;
  lNegZero:=-lZero;
  CheckSpecial(41,'ArcCscH',@DoubleCscH,lZero,Infinity);
  CheckSpecial(42,'ArcCscH',@DoubleCscH,lNegZero,NegInfinity);
  CheckSpecial(43,'ArcCscH',@DoubleCscH,Infinity,lZero);
  CheckSpecial(44,'ArcCscH',@DoubleCscH,NegInfinity,lNegZero);
  Check(45,IsNan(Call(@DoubleCscH,NaN,lError)) and (lError=''),'ArcCscH(NaN) is NaN');
  CheckSpecial(51,'ArcSecH',@DoubleSecH,lZero,Infinity);
  Check(52,IsNan(Call(@DoubleSecH,NaN,lError)) and (lError=''),'ArcSecH(NaN) is NaN');
  CheckSpecial(61,'ArcCotH',@DoubleCotH,1.0,Infinity);
  CheckSpecial(62,'ArcCotH',@DoubleCotH,-1.0,NegInfinity);
  CheckSpecial(63,'ArcCotH',@DoubleCotH,Infinity,lZero);
  Check(64,IsNan(Call(@DoubleCotH,NaN,lError)) and (lError=''),'ArcCotH(NaN) is NaN');

  lSingle:=-1e-9;
  Check(71,SameValue(ArcCscH(lSingle),Single(-21.416413),1e-5),'Single ArcCscH(-1e-9)');
  lSingle:=1e-30;
  Check(72,SameValue(ArcCscH(lSingle),Single(69.7707),1e-4),'Single ArcCscH(1e-30)');
  lSingle:=1e30;
  Check(73,SameValue(ArcCscH(lSingle),Single(1e-30),Single(1e-36)),'Single ArcCscH(1e30)');
  lSingle:=1e20;
  Check(74,SameValue(ArcCotH(lSingle),Single(1e-20),Single(1e-26)),'Single ArcCotH(1e20)');
{$ifdef FPC_HAS_TYPE_EXTENDED}
  lExt:=-1e-9;
  Check(81,SameValue(ArcCscH(lExt),Extended(-21.416413017506358),1e-13),'Extended ArcCscH(-1e-9)');
  lExt:=1e-4000;
  Check(82,SameValue(ArcCscH(lExt),Extended(9211.0335191567427),1e-12),'Extended ArcCscH(1e-4000)');
  lExt:=1e300;
  Check(83,SameValue(ArcCscH(lExt),Extended(1e-300),Extended(1e-313)),'Extended ArcCscH(1e300)');
{$endif}
  writeln('ok');
end.
