{ %fail }
var
  x :longint;
procedure test; nostackframe;
begin
  x:=78;
end;

begin
  x:=-1;
  test;
  if x<>78 then
    begin
      writeln('Wrong result in  nostackframe non-assembler procedure');
      halt(1);
    end
  else
    writeln('Pascal procedure nostackframe works OK');
end.
