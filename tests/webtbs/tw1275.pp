var
  sizes:array [1..3] of int64;
  i:integer;

function Count:int64;
var
  c:int64;
begin
  c:=1;

  writeln(c);
  Count:=c;
end;

begin
    i:=1;
    sizes[i]:=Count();
    writeln(sizes[i]);
end.
