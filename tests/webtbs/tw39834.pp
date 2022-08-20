{ %OPT=-O- -O2}
{$mode objfpc}
program DivTest;

function fquotient(a, b: integer): integer; inline;
begin
  result := a div b;
end;

procedure test;
var seconds, min: integer;
begin
  seconds := 3600 + 60 * 40 + 52;
  min      := fquotient(seconds, 60);
  WriteLn(min);
  if min<>100 then
    halt(1);
end;

begin
  test;
end.
