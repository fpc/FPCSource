{ %fail }
{$mode objfpc}
program tnameof9;
function f(x: Integer): Integer;
begin
  Result:=x;
end;

var
  s: String;
begin
  s:=NameOf(f(1));
end.
