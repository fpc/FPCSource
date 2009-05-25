var
  p: pointer;
begin
  p:=nil+1;
  if (p<>pointer(1)) then
    halt(1);
end.
