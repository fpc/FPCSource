{ %opt=-Sd }
var
  s : set of 0..7;
begin
  if sizeof(s)<>1 then
    halt(1);
  writeln('ok');
end.

