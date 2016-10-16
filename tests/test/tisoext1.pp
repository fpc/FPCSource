{$mode iso}
var
  f : file of byte;
  b : byte;
begin
  rewrite(f,'tisoext1.tmp');
  write(f,123);
  close(f);
  b:=0;
  reset(f,'tisoext1.tmp');
  read(f,b);
  if b<>123 then
    halt(1);
  close(f);
  writeln('ok');
end.

