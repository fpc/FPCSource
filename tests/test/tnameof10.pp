{ %fail }
{$mode objfpc}
program tnameof10;
var
  i: Integer;
var
  s: String;
begin
  s:=NameOf(Integer(i));
end.
