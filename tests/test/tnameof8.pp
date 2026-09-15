{ %fail }
{$mode objfpc}
program tnameof8;
var
  p: ^Integer;
var
  s: String;
begin
  s:=NameOf(p^);
end.
