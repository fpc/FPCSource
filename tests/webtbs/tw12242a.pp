{$mode objfpc}

type
  tc = class
    class procedure a; cdecl; static;
    class procedure b; cdecl; static;
    procedure c;
  end;

var
  ok: boolean;

class procedure tc.a; cdecl; static;
begin
  writeln('a');
  ok:=true;
end;


class procedure tc.b; cdecl; static;
begin
  a;
end;

procedure tc.c;
begin
  a;
end;

var
  c: tc;
begin
  ok:=false;
  tc.b;
  if not ok then
    halt(1);
  ok:=false;
  c:=tc.create;
  c.c;
  c.free;
  if not ok then
    halt(2);
end.
