{ %OPT=-O-G2 -Sd}

var
  c: array[1..10] of integer;
  b, a: array[1..10, 1..10] of integer;

procedure rec(k: integer);
var i: integer;
begin
  for i := 1 to c[k] do
    if a[k, b[k, i]] = 0 then
    begin
      //writeln(i, ' ', k);
      a[k, b[k, i]]:= 1;
      a[b[k, i], k]:= 1;
      rec(b[k, i]);
    end;
end;

begin
  fillchar(a, sizeof(a), 0);
  c[1] := 2;
  c[2] := 2;
  c[3] := 2;
  b[1, 1] := 2; b[1, 2] := 3;
  b[2, 1] := 1; b[2, 2] := 3;
  b[3, 1] := 1; b[3, 2] := 2;
  rec(1);
end.

