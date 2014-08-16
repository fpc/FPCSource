begin
  if not(qwordbool($8000000000000000)) then
    halt(1);
  if not(longbool($80000000)) then
    halt(1);
  if not(wordbool($8000)) then
    halt(1);
  writeln('ok');
end.

