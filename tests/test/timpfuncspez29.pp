{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  All generic parameters must be used in the paramter list
}

program timpfuncspez29;

type
  generic TArray<T> = array of T;

generic procedure Test<T>(aT: specialize TArray<integer>);
begin
end;

begin
  Test([1,2,3]);
end.