var
  i : int64;
  j, k: longint;
begin
  k:=64;
  for j:=6400 to 6464 do
    begin
      i:=j;
      if (i div 64) <> (i div k) then
        halt(1);
    end;
  i:=6500;
  i:=i div 65;
  if i<>100 then
    halt(1);
  for j:=-6400 downto -6464 do
    begin
      i:=j;
      if (i div 64) <> (i div k) then
        halt(2);
    end;
  writeln('ok');
end.
