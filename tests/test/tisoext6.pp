{ %fail }
{ this is not supposed to compile in non iso mode }
var
  f : text;
  s : array[0..10] of char;
begin
  rewrite(f,'tisoext4.tmp');
  write(f,'FPC');
  close(f);
  writeln('ok');
end.

