var
  i : int64;

begin
  i:=6400;
  i:=i div 64;
  if i<>100 then
    halt(1);
  i:=6500;
  i:=i div 65;
  if i<>100 then
    halt(1);
  i:=-6400;
  i:=i div 64;
  if i<>-100 then
    halt(1);
  writeln('ok');
end.
