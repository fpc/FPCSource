var i: integer;
    err : boolean;
begin
  for i:= -1 to -2 do
    begin
      writeln (i);
      err:=true;
    end;
 writeln;
 for i:= 1 to 0 do
    begin
      writeln (i);
      err:=true;
    end;
  if err then
    halt(1);
end.
