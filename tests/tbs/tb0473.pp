const
  w = $5000;
begin
  writeln(hi(w));
  if hi(w)<>$50 then
    begin
      writeln('Error!');
      halt(1);
    end;
end.
