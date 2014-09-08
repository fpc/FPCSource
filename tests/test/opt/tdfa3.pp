{ %OPT=-Oodfa -Sew -vw -Sg }
{ %fail }
{ %norun  }
var
  i : longint;
label
  l;
begin
  goto l;
  i:=1;
  l:
  writeln(i);
end.

