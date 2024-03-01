var
  p : pointer;
begin
  p:=getmem(0);
  if not(assigned(p)) then
    halt(1);
  freemem(p);
end.
