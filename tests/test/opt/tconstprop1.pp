const
  l = 1 shl 63;

begin
  if l<>$8000000000000000 then
    halt(1);
end.

