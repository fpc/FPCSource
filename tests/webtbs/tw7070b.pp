{ %norun }

{$ifdef fpc}
{$mode delphi}
{$h+}
{$endif}

uses
  Variants;

procedure test(const a: array of variant);
begin
end;

var
  a,b: longint;
begin
  a:=1;
  b:=2;
  test([a,b]);
end.
