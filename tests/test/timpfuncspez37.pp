{ %FAIL }

program timpfuncspez37;

{$mode objfpc}
{$modeswitch implicitfunctionspecialization}

type
  generic TTestFunc<T> = procedure(aArg1: T);

generic procedure DoTest<T>(aArg: specialize TTestFunc<T>);
begin
end;

function TestFunc(aArg1: LongInt): LongInt;
begin
end;

begin
  DoTest(@TestFunc);
end.
