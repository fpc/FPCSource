{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Test default parameters
  NOTE: currently default values are not considered for specialization so the test should fail
}

program timpfuncspez17;

generic procedure DoThis<T>(a: integer; b: T = 0);
begin
end;

begin
  DoThis(1);
end.