var
  e: extended;
begin
  e := pi;
  if (sizeof(single) < sizeof(extended)) and
     (single(e) = e) then
    halt(1);
end.
