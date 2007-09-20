{ %OPT=-Ooloopunroll }
var
 arr : array[0..0,0..0] of longint;
 i : longint;

begin
  i:=1234;
  for i := 0 to 0 do
    if i<>0 then
      halt(1);
  writeln('ok');
end.
