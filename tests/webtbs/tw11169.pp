var
  l: longint;
begin
  for l:=1 to 255 do
    if paramstr(l) <> '' then
      halt(1);
end.
