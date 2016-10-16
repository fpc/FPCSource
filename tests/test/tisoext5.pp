{ %fail }
{ this is not supposed to compile in non iso mode }
var
  f : text;
  s : array[0..10] of char;
begin
  reset(f,'tisoext4.tmp');
  read(f,s);
  if s<>'FPC' then
    halt(1);
  close(f);
  writeln('ok');
end.

