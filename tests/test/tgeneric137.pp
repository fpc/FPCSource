{ a parameterless routine of an earlier scope compared with "<" next to a
  generic-only routine of the same name in a later one }
program tgeneric137;

{$mode delphi}

uses
  ugeneric137a, ugeneric137b;

function Lim5<T>(a: T): T;
begin
  Result := a;
end;

var
  e: Integer;
begin
  e := 0;
  if not (Lim2 < 30) then
    e := e or 1;
  if Lim2 < 10 then
    e := e or 2;
  if not (Lim5 < 60) then
    e := e or 4;
  if Lim2<Integer>(7) <> 7 then
    e := e or 8;
  if Lim5<Integer>(9) <> 9 then
    e := e or 16;
  if e <> 0 then
    begin
      Writeln('failed ', e);
      Halt(e);
    end;
  Writeln('ok');
end.
