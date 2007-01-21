type
{$z-}
  te1 = (a,b,c);
{$z+}
  te2 = (e,f,g);
begin
  if sizeof(te1) <> 1 then
    halt(1);
  if sizeof(te2) <> 4 then
    halt(2);
end.
