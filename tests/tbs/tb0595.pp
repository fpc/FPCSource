{$mode delphi}{$h+}

type
  tc = class
    class procedure test; static;
  end;

  tp = procedure;

var
  global: longint;

  class procedure tc.test;
    begin
      global:=1;
    end;

var
  p: tp;
begin
  p:=tp(tc.test);
  p();
  if global<>1 then
    halt(1);
end.
