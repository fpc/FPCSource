var
  l: longint;
  a: array[0..1] of char;

begin
  l := 50;
  str(l,a);
  if a <> '50' then
    begin
      writeln('error');
      halt(1);
    end;
end.
