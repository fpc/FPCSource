{ %fail }
{$mode objfpc}
program tnameof5;
var
  a: array[1..2] of Integer;
var
  s: String;
begin
  s:=NameOf(a[1]);
end.
