{ %opt=-O2 }

type
  tr = record
    a,b: longint;
  end;

function f: tr;
begin
  f.a:=5;
  f.b:=6;
end;

procedure test;
var
  r: tr;
begin
  r.a:=1;
  r.b:=2;
  r.a:=f.a;
  writeln(r.a);
  writeln(r.b);
  if (r.a<>5) then
    halt(1);
  if (r.b<>2) then
    halt(2);
end;

begin
  test;
end.
