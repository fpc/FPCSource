{%FAIL}
{$mode objfpc}
{$modeswitch advancedrecords}
{
  test binary operator error with wrong constant type
}
program tgenconst15;

type
  generic TInt<const I: string> = record
    const c = I div I;
  end;

begin
end.