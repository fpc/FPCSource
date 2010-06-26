{ %opt=-g-t }
program project1;
{$mode objfpc}{$H+}

uses Classes;

type
TBar = class
  function foo(StockID: LongInt; out Image, Mask: Longint): Boolean;
end;

function TBar.foo(StockID: LongInt; out Image, Mask: Longint): Boolean;
begin
  Result := False;
end;

var
  a: array[0..10000] of longint;
  i: longint;
  x:TBar;
begin
  x.foo(0,a[0],a[1]);
  for i:=2 to high(a) do
    if a[i]<>0 then
      halt(1);
end.
