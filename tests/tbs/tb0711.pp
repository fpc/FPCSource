{ %opt=-O- -O1 }
var
  l : longint;

begin
  l:=1234;

  if -l-1<>-1235 then
    halt(1);

  if -(l+1)<>-1235 then
    halt(2);

  if -(1+l)<>-1235 then
    halt(3);

  if -1-l<>-1235 then
    halt(4);
end.