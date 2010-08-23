unit uw17220;
{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}
interface

function IntToHEX(Value, Digits: int64): string; overload;

implementation

function IntToHEX(Value, Digits: int64): string;
begin
  IntToHEX := 'passed';
end;

end.

