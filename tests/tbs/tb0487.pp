uses
  variants;
var
  v : variant;
  i : longint;

begin
  v:=true;
  if not(v) then
    halt(1);
  while not(v) do
    halt(1);
  i:=1;
  repeat
    if i>1 then
      halt(1);
    inc(i);
  until v;
  writeln('ok');
end.
