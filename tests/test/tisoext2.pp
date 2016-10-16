{ %fail }
{ this is not supposed to compile in non iso mode }
var
  f : file of byte;
  b : byte;
begin
  reset(f,'tisoext1.tmp');
  read(f,b);
  if b<>123 then
    halt(1);
  close(f);
  writeln('ok');
end.

