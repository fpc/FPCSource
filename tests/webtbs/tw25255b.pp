var
  qb : qwordbool;
  lb : longbool;
  wb : wordbool;
procedure p;
begin
  if not(qb) then
    halt(1);
  if not(lb) then
    halt(1);
  if not(wb) then
    halt(1);
end;

begin
  qb:=qwordbool($8000000000000000);
  lb:=longbool($80000000);
  wb:=wordbool($8000);
  p;
  writeln('ok');
end.
