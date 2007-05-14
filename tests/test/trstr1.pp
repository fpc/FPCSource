var
  s: ansistring;
  i,j: integer;
  c1,c2: char;
begin
  s := '15';
  { temp ansistring must be kept until read is finished }
  readstr(s+' ,305',i,c1,c2,j);
  if (i <> 15) or
     (c1 <> ' ') or
     (c2 <> ',') or
     (j <> 305) then
    halt(1);
end.

