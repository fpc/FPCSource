unit uw17220a;
{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}
interface

uses
  SysUtils;

procedure test;
function IntToHEX(Value, Digits: int64): string; overload;

implementation

function IntToHEX(Value, Digits: int64): string;
begin
  IntToHEX := 'passedq';
end;

function IntToHEX(Value, Digits: longint): string; overload;
begin
  IntToHEX := 'passedl';
end;

procedure test;
  var
    l: longint;
    i: int64;
  begin
    l:=0;
    i:=0;
    if uw17220a.inttohex(l,l)<>'passedl' then
      halt(1);
    if uw17220a.inttohex(i,i)<>'passedq' then
      halt(2);
  end;

end.

