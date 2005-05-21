var
  a : array[0..9] of char;
  pc : pchar;
begin
  a:='1';
  if a=nil then
   halt(1);
  pc:=@a;
  if pc<>'1' then
   halt(1);
  writeln('OK')
end.
