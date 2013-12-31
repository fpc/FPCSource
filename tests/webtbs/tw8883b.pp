{ %OPT=-Oonoconstprop }
procedure DoTest;
var
  i, j, cnt: longint;
begin
  cnt:=0;
  j:=1;
  for i:=0 to j do
  begin
    Inc(cnt);
    Dec(j);
  end;

  writeln(cnt);
  if cnt <> 2 then
  begin
    writeln('Test failed!');
    Halt(1);
  end;
  writeln('Test OK.');
end;

begin
  dotest;
end.
