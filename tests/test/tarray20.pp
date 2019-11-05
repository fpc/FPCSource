{ %FAIL }

program tarray20;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
  end;

operator Explicit(a: array of LongInt): TTest;
begin

end;

var
  t: TTest;
begin
  t := [21, 42];
end.
