{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Test specializing proc vars with constant parameters
  NOTE: currently not supported and therefore should fail
}

program timpfuncspez15;

type
  generic TProc<T; const U: integer> = procedure(value: T = U);

generic procedure Run<T>(proc: specialize TProc<T, 10>);
begin
  proc();
end;

procedure DoCallback(value: integer);
begin
end;

begin
  Run(@DoCallback);
end.