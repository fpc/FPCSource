var i : char;
    bb: bytebool;
begin
  boolean(i) := (1=1);
  if not boolean(i) then
    halt(1);
  boolean(bb):=boolean(i); 
  if not(bb) then
    halt(2);
end.
