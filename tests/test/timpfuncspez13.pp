{%NORUN}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Test specializing proc vars by infering parameters from callback function
}

program timpfuncspez13;

type
  generic TProc<S, V> = procedure(name: S; context: V);

generic procedure Run<U, T>(proc: specialize TProc<U, T>; context: T);
begin
  proc('TProc', context);
end;

procedure DoCallback(name: string; value: shortint);
begin
  writeln(name, ' called ', value);
end;

begin
  Run(@DoCallback, 100);
end.