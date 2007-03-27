{ %fail }

program testbug;

{$mode objfpc}{$H+}

type
  TState = (a, b, c, d);
  TStates = set of TState;

var
  s : TStates;
begin
  s := [a];
  if s = a then
    writeln('Should give a compile time error');

  if s = b then halt(1);

  if s = [a] then
    writeln('OK');
end.

