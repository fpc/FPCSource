{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Parameter count must match in order for specialization to be considered
}

program timpfuncspez26;


generic procedure Test<T>(aT: T);
begin
end;

begin
  Test(1, 1);
end.