{ %fail }
{$mode objfpc}
program tnameof7;
var
  i: Integer;
var
  s: String;
begin
  s:=NameOf(i,i);
end.
