var
  value:real;
  fin:text;
begin
  assign(fin,'tw1896.tmp');
  rewrite(fin);
  writeln(fin,'12.3');
  writeln(fin,'13.2');
  close(fin);

  assign(fin,'tw1896.tmp');
  reset(fin);
  while not eof(fin) do
  begin
  read(fin,value);
  writeln(value)
  end;
  { Delphi returns 0 as last value }
  if value<>0 then
   begin
     writeln('Error');
     halt(1);
   end;
  close(fin);
  erase(fin);
end.
