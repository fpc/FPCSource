program tw40972;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cwstring,
{$endif}
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


// Checks that aText is rejected as a Double, Single, Extended and Real
procedure CheckOverflow(const aText: string; const aFormatSettings: TFormatSettings);

var
  lDouble: Double;
  lSingle: Single;
  lExtended: Extended;
  lReal: Real;
  lText: AnsiString;

begin
  Check(not TryStrToFloat(aText,lDouble,aFormatSettings),'TryStrToFloat Double rejects '+aText);
  Check(not TryStrToFloat(aText,lSingle,aFormatSettings),'TryStrToFloat Single rejects '+aText);
  Check(not TryStrToFloat(aText,lExtended,aFormatSettings),'TryStrToFloat Extended rejects '+aText);
  Check(StrToFloatDef(aText,5,aFormatSettings)=5,'StrToFloatDef returns the default for '+aText);
  lText:=aText;
  Check(not TextToFloat(PAnsiChar(lText),lDouble,fvDouble,aFormatSettings),'TextToFloat fvDouble rejects '+aText);
  Check(not TextToFloat(PAnsiChar(lText),lSingle,fvSingle,aFormatSettings),'TextToFloat fvSingle rejects '+aText);
  Check(not TextToFloat(PAnsiChar(lText),lReal,fvReal,aFormatSettings),'TextToFloat fvReal rejects '+aText);
  Check(not TextToFloat(PAnsiChar(lText),lExtended,fvExtended,aFormatSettings),'TextToFloat fvExtended rejects '+aText);
end;


var
  lFS: TFormatSettings;
  lDouble, lMax: Double;
  lSingle: Single;
  lExtended: Extended;
  lRaised: Boolean;

begin
  lFS:=DefaultFormatSettings;
  lFS.DecimalSeparator:='.';
  lFS.ThousandSeparator:=',';
  CheckOverflow('1E5000',lFS);
  CheckOverflow('-1E5000',lFS);
  CheckOverflow(' 1E5000 ',lFS);
  CheckOverflow('.5E5000',lFS);
  Check(not TryStrToFloat('1E400',lDouble,lFS),'TryStrToFloat Double rejects 1E400');
  Check(not TryStrToFloat('1.79769313486232E308',lDouble,lFS),'TryStrToFloat Double rejects 1.79769313486232E308');
  Check(not TryStrToFloat('1E39',lSingle,lFS),'TryStrToFloat Single rejects 1E39');
  lRaised:=False;
  try
    StrToFloat('1E5000',lFS);
  except
    on E: EConvertError do
      lRaised:=True;
  end;
  Check(lRaised,'StrToFloat raises EConvertError for 1E5000');

  lMax:=Double.MaxValue;
  Check(TryStrToFloat('1.7976931348623157E308',lDouble,lFS) and (lDouble=lMax),'TryStrToFloat Double accepts MaxValue');
  Check(TryStrToFloat('Inf',lDouble,lFS) and lDouble.IsPositiveInfinity,'TryStrToFloat Double accepts Inf');
  Check(TryStrToFloat('+Inf',lDouble,lFS) and lDouble.IsPositiveInfinity,'TryStrToFloat Double accepts +Inf');
  Check(TryStrToFloat('-Inf',lDouble,lFS) and lDouble.IsNegativeInfinity,'TryStrToFloat Double accepts -Inf');
  Check(TryStrToFloat('Nan',lDouble,lFS) and lDouble.IsNan,'TryStrToFloat Double accepts Nan');
  Check(TryStrToFloat('-Inf',lExtended,lFS) and lExtended.IsNegativeInfinity,'TryStrToFloat Extended accepts -Inf');
  Check(TryStrToFloat('Inf',lSingle,lFS) and lSingle.IsPositiveInfinity,'TryStrToFloat Single accepts Inf');

  lFS.DecimalSeparator:=',';
  lFS.ThousandSeparator:='.';
  CheckOverflow('1,5E5000',lFS);
  Check(TryStrToFloat('1,5',lDouble,lFS) and (lDouble=1.5),'TryStrToFloat Double accepts 1,5');

  if gFailed then
    Halt(1);
  Writeln('ok');
end.
