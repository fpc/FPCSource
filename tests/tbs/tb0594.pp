{$ifdef fpc}
{$mode delphi}
{$endif}

type
{$ifndef fpc}
  codepointer = pointer;
{$endif}
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
  if codepointer(@p)<>tmethod(p2).code then
    halt(1);
end.
