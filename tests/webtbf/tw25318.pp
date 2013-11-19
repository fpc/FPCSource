{%FAIL}
{$mode objfpc}

var
  A: array[1..10] of Integer;
  E: Integer;
begin
  for E in A do
    E:=1;
end.
