{ %target=linux,darwin,freebsd,win32,win64 }
{ TFormatSettings.Create with a locale name }
program tfslocale;

{$mode objfpc}{$H+}

uses
{$ifdef unix}
  clocale,
{$endif}
  SysUtils;

var
  gErrors: Integer = 0;


procedure CheckEqual(const aName, aValue, aExpected: string);

begin
  if aValue=aExpected then
    Writeln(aName,' OK')
  else
    begin
    Writeln(aName,' ERROR: got "',aValue,'", expected "',aExpected,'"');
    Inc(gErrors);
    end;
end;


// Creates the format settings of aLocale; False if the locale is not available.
function TryCreate(const aLocale: string; out aSettings: TFormatSettings): Boolean;

begin
  try
    aSettings:=TFormatSettings.Create(aLocale);
    Result:=True;
  except
    on E: EArgumentException do
      Result:=False;
  end;
end;


procedure CheckGerman(const aLocale: string);

var
  lFS: TFormatSettings;

begin
  if not TryCreate(aLocale,lFS) then
    begin
{$ifdef mswindows}
    CheckEqual(aLocale+' available','False','True');
{$else}
    Writeln(aLocale,' not installed, skipped');
{$endif}
    Exit;
    end;
  CheckEqual(aLocale+' DecimalSeparator',lFS.DecimalSeparator,',');
  CheckEqual(aLocale+' FloatToStr',FloatToStr(1234.5,lFS),'1234,5');
  CheckEqual(aLocale+' FormatFloat',FormatFloat('#,##0.00',1234.5,lFS),'1.234,50');
  CheckEqual(aLocale+' DateSeparator',FormatDateTime('dd/mm/yyyy',EncodeDate(2024,3,5),lFS),'05.03.2024');
  CheckEqual(aLocale+' LongMonthNames[1]',lFS.LongMonthNames[1],'Januar');
  CheckEqual(aLocale+' LongDayNames[1]',lFS.LongDayNames[1],'Sonntag');
  CheckEqual(aLocale+' ListSeparator set',BoolToStr(lFS.ListSeparator<>#0,True),'True');
end;


var
  lFS: TFormatSettings;
  lDecimalSeparator: Char;
  lShortDateFormat, lMonthName: string;
  lRaised: Boolean;

begin
  lDecimalSeparator:=DefaultFormatSettings.DecimalSeparator;
  lShortDateFormat:=DefaultFormatSettings.ShortDateFormat;
  lMonthName:=DefaultFormatSettings.LongMonthNames[1];
  lRaised:=False;
  try
    lFS:=TFormatSettings.Create('xx-YY');
  except
    on E: EArgumentException do
      lRaised:=True;
  end;
  CheckEqual('Unknown locale raises EArgumentException',BoolToStr(lRaised,True),'True');
  CheckGerman('de-DE');
{$ifdef unix}
  CheckGerman('de_DE.UTF-8');
{$endif}
  CheckGerman('de-DE');
  CheckEqual('DefaultFormatSettings.DecimalSeparator unchanged',DefaultFormatSettings.DecimalSeparator,lDecimalSeparator);
  CheckEqual('DefaultFormatSettings.ShortDateFormat unchanged',DefaultFormatSettings.ShortDateFormat,lShortDateFormat);
  CheckEqual('DefaultFormatSettings.LongMonthNames[1] unchanged',DefaultFormatSettings.LongMonthNames[1],lMonthName);
  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
