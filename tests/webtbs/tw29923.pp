{$mode objfpc}

{$r+}

type
  tc = class
    f: longint;
    procedure test;
  end;

procedure tc.test;

procedure nest;
var
  l1: longint;
begin
  f:=-1;
  l1:=-1;
  dec(f,l1*120);
end;

begin
  nest;
end;

var
  c: tc;
begin
  c:=tc.create;
  c.test;
  c.free;
end.
