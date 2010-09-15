{ %opt=-O2 }

type
  tr = record
    a: array[0..1] of longint;
  end;

function f: tr;
begin
  f.a[0]:=5;
  f.a[1]:=6;
end;

procedure test;
var
  r: tr;
begin
  r.a[0]:=1;
  r.a[1]:=2;
  r.a[0]:=f.a[0];
  writeln(r.a[0]);
  writeln(r.a[1]);
  if (r.a[0]<>5) then
    halt(1);
  if (r.a[1]<>2) then
    halt(2);
end;

begin
  test;
end.
