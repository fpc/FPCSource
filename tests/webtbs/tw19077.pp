uses
  math;
var
  c : currency;
begin
  c:=1.5625;
  c:=RoundTo(c,-1);
  if c<>1.6 then
    halt(1);
  writeln('ok');
end.


