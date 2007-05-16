{ %norun }

{$ifdef fpc}
{$mode delphi}
{$h+}
{$endif}

uses
  Variants;

procedure test(const a: array of string);
begin
end;

var
  a,b: variant;
begin
  a:=1;
  b:=2;
  test([a,b]);
end.
