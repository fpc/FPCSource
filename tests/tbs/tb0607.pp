{ %opt=-O2 }

var
  a,b: longword;
  i: int64;
  l1, l2: longword;
begin
  a:=1;
  b:=123456;
  i:= (a-b) div 10;
  l1:=longword(i);
  l2:=longword((a-b) div 10);
  writeln(l1);
  writeln(l2);
  if l1<>l2 then
    halt(1);
end.
