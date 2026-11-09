{$mode delphi}

program tw38216;

uses
  uw38216a, uw38216b;

var
  i: Integer;
  s: string;

begin
  i := Test2<Integer>.TestFunction;
  if i <> 0 then
    halt(1);
  s := Test2<string>.TestFunction;
  if s <> '' then
    halt(2);
  writeln('ok');
end.
