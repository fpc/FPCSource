uses
  sysutils;
var
  s : string;
begin
  str(currency(25.996):0:2,s);
  if s<>'26.00' then
    halt(1);
  str(currency(99.996):0:2,s);
  if s<>'100.00' then
    halt(1);
  writeln('ok');
end.
