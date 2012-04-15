{ %FAIL }
{ %target=darwin }

{ Objective C types are disallowed as well }
program tdefault16;

{$mode objfpc}
{$modeswitch objectivec1}

type
  TTest = objcprotocol
  end;

var
  t: TTest;
begin
  t := Default(TTest);
end.
