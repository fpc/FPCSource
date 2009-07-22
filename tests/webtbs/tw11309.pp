{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  SysUtils;

var
  csMoney : string;

Function Format_Currency_String1(sMoney : string) : string;
var
aCurrency : Currency;
begin
  TRY
aCurrency := strtoCurr(sMoney);
  EXCEPT
   on E: EConvertError do aCurrency := 0;
  END;
//result := CurrToStrF(currBetrag,ffFixed,2);
result := FloatToStrF(aCurrency,ffFixed,19,2);
end;

Function Format_Currency_String2(sMoney : string) : string;
var
aCurrency : real;
begin
  TRY
aCurrency := strtofloat(sMoney);
  EXCEPT
   on E: EConvertError do aCurrency := 0;
  END;
result := FloatToStrF(aCurrency,ffFixed,19,2);
end;

begin
  csMoney:='58'+DecimalSeparator+'195';
  writeln(Format_Currency_String1(csMoney));
  writeln(Format_Currency_String2(csMoney));
  if Format_Currency_String1(csMoney)<>'58'+DecimalSeparator+'20' then
    halt(1);
  if Format_Currency_String2(csMoney)<>'58'+DecimalSeparator+'20' then
    halt(2);
  writeln('ok');
end.
