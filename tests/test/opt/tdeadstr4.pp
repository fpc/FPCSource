{ %OPT=-O3 -Oodeadstore }
uses
  udeadstr4;

begin
  if i<>1234 then
    halt(1);
  writeln('ok');
end.
