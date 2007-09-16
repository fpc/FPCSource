var
  s: string;
  d: Double;
begin
  d := 5.9999999999999991;
  Str(d:23,s); 
  if (pos('9',s)<>0) or (pos('5',s)<>0) then
    halt(1);
end.
