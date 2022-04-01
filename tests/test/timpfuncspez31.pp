{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  All generic parameters must be used in the paramter list
}

program timpfuncspez31;

type
  generic TProc<S> = procedure(name: S);

generic procedure Run<T>(proc: specialize TProc<String>);
begin
  proc('TProc');
end;

procedure DoCallback(name: String);
begin
  writeln(name, ' called');
end;

begin
  Run(@DoCallback);
end.