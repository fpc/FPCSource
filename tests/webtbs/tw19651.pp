{$mode objfpc}{$h+}
{$APPTYPE CONSOLE}

uses
  SysUtils;

resourcestring
  SSunday = 'Sunday';

const
  SDays: array[0..0] of string = (SSunday);

function Translate(Name,Value : AnsiString; Hash : Longint; arg:pointer) : AnsiString;
begin
  Result := 'dimanche';
end;

begin
  SetResourceStrings(@Translate, nil);
  WriteLn(SSunday);
  WriteLn(SDays[0]);
  if SDays[0]<>'dimanche' then
    Halt(1);
end.
