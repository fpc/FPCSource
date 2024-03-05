var
  l : longint;

begin
  l:=1234;

  if -l-1<>-1235 then
    halt(1);
end.