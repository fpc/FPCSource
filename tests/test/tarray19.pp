{ %FAIL }

program tarray19;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
    class operator Explicit(a: array of LongInt): TTest;
  end;

class operator TTest.Explicit(a: array of LongInt): TTest;
begin

end;

var
  t: TTest;
begin
  t := [21, 42];
end.
