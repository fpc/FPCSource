uses
  Math, StrUtils;
begin
  if (IfThen(1 > 2, 1, 2) <> 2) then
    halt(1);
  if (IfThen(1 > 2, '1', '2') <> '2') then
    halt(2);
  if (IfThen(1 > 2, '123', '456') <> '456') then
    halt(3);
end.
