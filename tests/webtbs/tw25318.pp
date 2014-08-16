{%NORUN}
{$mode objfpc}

type
  PInteger = ^Integer;
var
  A: array[1..10] of PInteger;
  E: PInteger;
begin
  for E in A do
    E^:=1;
end.
