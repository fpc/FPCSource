CONST C : Currency = 100;
var
  s : string;

begin
  str(C:10:2,s);
  if s<>'    100.00' then
    halt(1);
  writeln('ok');
end.
