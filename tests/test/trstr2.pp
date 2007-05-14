var
  s: ansistring;
  i,j: integer;
begin
  s := '15 305';
  readstr(s,i,j);
  if (i <> 15) or
     (j <> 305) then
    halt(1);
end.
