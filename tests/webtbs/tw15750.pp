program boolloop;
var
  i: longint;
  a, b: boolean;
begin
  a:=true;
  i:=0;
  for b:=not a to true do
    begin
      inc(i);
      writeln('now b is ',b);
    end;
  if i<>2 then
    halt(1);
end.

