var
  a : array[0..9] of char;
  pc : pchar;
begin
  a:='1';
  pc:=@a;
  if pc='1' then
    writeln('OK')
  else
    halt(1);
end.

