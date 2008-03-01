var
  b: byte;
  s: shortint;
  l: longint;
  c: cardinal;
  i: int64;
begin
  b:=10;
  s:=-128;
  l:=b or s;
  writeln(l);
  if (l<>-118) then
    halt(1);
  l:=b xor s;
  writeln(l);
  if (l<>-118) then
    halt(2);
  b:=129;
  s:=-127;
  l:=b and s;
  writeln(l);
  if (l<>129) then
    halt(3);
  l:=s and b;
  writeln(l);
  if (l<>129) then
    halt(4);

  l:=-127;
  c:=129;
  i:=l and c;
  writeln(i);
end.
