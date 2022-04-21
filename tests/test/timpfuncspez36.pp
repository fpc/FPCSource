{ %NORUN }

program timpfuncspez36;

{$mode objfpc}
{$modeswitch implicitfunctionspecialization}

type
  generic TTestFunc<T> = procedure(aArg1: String; aArg2: T);

generic procedure DoTest<T>(aArg: specialize TTestFunc<T>);
begin
end;

procedure TestFunc(aArg1: String; aArg2: LongInt);
begin
end;

begin
  DoTest(@TestFunc);
end.
