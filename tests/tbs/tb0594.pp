{$ifdef fpc}
{$mode delphi}
{$endif}

type
  tc = class
    class procedure test;
  end;

  tp = procedure;
  tp2 = procedure of object;

class procedure tc.test;
begin
end;

var
  p: tp;
  p2: tp2;
begin
  p:=tp(tc.test);
  p2:=tc.test;
  if pointer(@p)<>tmethod(p2).code then
    halt(1);
end.
