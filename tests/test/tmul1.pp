var
  i : longint;

begin
  i:=5;
  i:=i*10;
  i:=i*62;
  i:=i*-10;
  i:=i*-62;
  i:=i*87;
  if i<>167214000 then
    halt(1);
  writeln('ok');
end.
