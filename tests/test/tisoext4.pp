{$mode iso}
var
  f : text;
  s : array[0..10] of char;
begin
  rewrite(f,'tisoext4.tmp');
  write(f,'FPC');
  close(f);
  reset(f,'tisoext4.tmp');
  read(f,s);
  if s<>'FPC' then
    halt(1);
  close(f);
  writeln('ok');
end.

