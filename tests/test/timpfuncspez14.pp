{%NORUN}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Test specializing proc vars by infering parameters from callback function
}

program timpfuncspez14;

type
  generic TProc<S> = procedure(name: S; value: integer);

generic procedure Run<T, U>(proc: specialize TProc<T>; context: U);
begin
  proc('TProc', context);
end;

procedure DoCallback(name: string; value: integer);
begin
  writeln(name, ' called ', value);
end;

begin
  Run(@DoCallback, 100);
end.