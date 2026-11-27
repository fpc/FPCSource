{ %FAIL }
{ sets can have at most 65536 elements }
{$mode objfpc}
program tset17;

type
  t = set of 0..65536;

begin
end.
