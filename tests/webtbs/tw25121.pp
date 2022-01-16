{ %OPT=-Oonofastmath }

begin
  if not(single(144115188075855877) = single(144115188075855872)) then
    halt(1);
  if not(single(1 / 3.0) = single(0.33333333)) then
    halt(2);
  if not(single(92233720368547758) = single(92233720000000000)) then
    halt(3);
end.
