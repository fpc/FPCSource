program fail;

var
  i : int64;
  err : word;
begin
  if paramstr(1)<>'' then
    begin
      val(paramstr(1),i,err);
      if err<>0 then
        writeln('Error with ',paramstr(1))
      else
        halt(i);
    end;
  halt(1);
end.
