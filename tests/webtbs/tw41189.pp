program tw41189;

{$mode objfpc}{$h+}

uses
  SysUtils;

var
  gFailed: Boolean = False;


// Reports aMessage as a failure when aCondition is false
procedure Check(aCondition: Boolean; const aMessage: string);

begin
  if not aCondition then
    begin
    Writeln('failed: ',aMessage);
    gFailed:=True;
    end;
end;


// Checks that aActual equals aExpected
procedure CheckStr(const aActual, aExpected, aMessage: string);

begin
  Check(aActual=aExpected,aMessage+': expected "'+aExpected+'", got "'+aActual+'"');
end;


{$ifdef FPC_HAS_TYPE_EXTENDED}
// Checks that aValue written with 21 significant digits reads back unchanged
procedure CheckExtendedRoundTrip(const aName: string; aValue: Extended; const aFormatSettings: TFormatSettings);

var
  lText: string;
  lBack: Extended;

begin
  lText:=FloatToStrF(aValue,ffGeneral,21,0,aFormatSettings);
  Check(TryStrToFloat(lText,lBack,aFormatSettings) and (lBack=aValue),aName+' reads back unchanged from '+lText);
end;


// Checks the Extended precision of FloatToStrF and FormatFloat
procedure CheckExtended(const aFormatSettings: TFormatSettings);

var
  lThird, lTwoThirds: Extended;
  lRec: TFloatRec;

begin
  lThird:=1;
  lThird:=lThird/3;
  lTwoThirds:=2;
  lTwoThirds:=lTwoThirds/3;
  FloatToDecimal(lRec,lThird,50,50);
  Check(IndexByte(lRec.Digits,SizeOf(lRec.Digits),0)>=0,'FloatToDecimal Extended precision 50 Digits ends with #0');
  if IndexByte(lRec.Digits,SizeOf(lRec.Digits),0)>=0 then
    CheckStr(StrPas(@lRec.Digits[0]),'3333333333333333333','FloatToDecimal Extended precision 50 Digits');
  CheckStr(FloatToStrF(lThird,ffGeneral,21,0,aFormatSettings),'0.333333333333333333342','FloatToStrF Extended ffGeneral 21');
  CheckStr(FloatToStrF(lThird,ffGeneral,25,0,aFormatSettings),'0.333333333333333333342','FloatToStrF Extended ffGeneral 25');
  CheckStr(FloatToStrF(lThird,ffGeneral,18,0,aFormatSettings),'0.333333333333333333','FloatToStrF Extended ffGeneral 18');
  CheckStr(FloatToStrF(lThird,ffGeneral,-1,0,aFormatSettings),'0.33333333333333333','FloatToStrF Extended ffGeneral -1');
  CheckStr(FloatToStrF(lThird,ffExponent,-1,3,aFormatSettings),'3.3333333333333333E-001','FloatToStrF Extended ffExponent -1');
  CheckStr(FloatToStrF(lThird,ffExponent,21,0,aFormatSettings),'3.33333333333333333342E-1','FloatToStrF Extended ffExponent 21');
  CheckStr(FloatToStr(lThird,aFormatSettings),'0.333333333333333','FloatToStr Extended');
  CheckStr(FormatFloat('0.0000000000000000000',lThird,aFormatSettings),'0.3333333333333333333','FormatFloat Extended 1/3');
  CheckStr(FormatFloat('0.###################',lTwoThirds,aFormatSettings),'0.6666666666666666667','FormatFloat Extended 2/3');
  CheckStr(FormatFloat('0.00',lTwoThirds,aFormatSettings),'0.67','FormatFloat Extended 2 decimals');
  CheckStr(FormatFloat('0.000000000000000000E+00',lTwoThirds,aFormatSettings),'6.666666666666666667E-01','FormatFloat Extended scientific');
  CheckExtendedRoundTrip('1/3',lThird,aFormatSettings);
  CheckExtendedRoundTrip('2/3',lTwoThirds,aFormatSettings);
  CheckExtendedRoundTrip('Pi',Pi,aFormatSettings);
  CheckExtendedRoundTrip('MaxValue',Extended.MaxValue,aFormatSettings);
  CheckExtendedRoundTrip('Epsilon',Extended.Epsilon,aFormatSettings);
end;
{$endif FPC_HAS_TYPE_EXTENDED}


var
  lFS: TFormatSettings;
  lDouble: Double;
  lSingle: Single;

begin
  lFS:=DefaultFormatSettings;
  lFS.DecimalSeparator:='.';
  lFS.ThousandSeparator:=',';
  lDouble:=1;
  lDouble:=lDouble/3;
  CheckStr(FloatToStrF(lDouble,ffGeneral,17,0,lFS),'0.33333333333333331','FloatToStrF Double ffGeneral 17');
  CheckStr(FloatToStrF(lDouble,ffGeneral,21,0,lFS),'0.33333333333333331','FloatToStrF Double ffGeneral 21');
  CheckStr(FloatToStrF(lDouble,ffExponent,21,0,lFS),'3.3333333333333331E-1','FloatToStrF Double ffExponent 21');
  CheckStr(FloatToStr(lDouble,lFS),'0.333333333333333','FloatToStr Double');
  lSingle:=0.5;
  CheckStr(FloatToStrF(lSingle,ffGeneral,21,0,lFS),'0.5','FloatToStrF Single ffGeneral 21');
{$ifdef FPC_HAS_TYPE_EXTENDED}
  CheckExtended(lFS);
{$endif FPC_HAS_TYPE_EXTENDED}
  if gFailed then
    Halt(1);
  Writeln('ok');
end.
