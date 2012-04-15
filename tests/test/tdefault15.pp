{ %FAIL }
{ %target=darwin }

{ Objective C types are disallowed as well }
program tdefault15;

{$mode objfpc}
{$modeswitch objectivec1}

type
  TTest = objcclass
  end;

var
  t: TTest;
begin
  t := Default(TTest);
end.
