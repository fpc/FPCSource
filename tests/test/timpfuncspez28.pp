{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  All generic parameters must be used in the paramter list
}

program timpfuncspez28;

generic procedure Test<T>(aT: array of integer);
begin
end;

begin
  Test([1,2,3]);
end.