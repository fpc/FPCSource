{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  All generic parameters must be used in the paramter list
}

program timpfuncspez27;

generic procedure Test<T>(aT: Integer);
begin
end;

begin
  Test(1);
end.