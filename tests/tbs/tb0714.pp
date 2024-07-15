var
  s : string[20];
begin
  setlength(s,21);
  if length(s)>sizeof(s)-1 then
    halt(1);
end.
