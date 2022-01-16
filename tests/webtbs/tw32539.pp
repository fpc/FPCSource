uses
  variants;
var
  v : variant;

begin
  v:=1.5;
  v:=sqr(v);
  if v<>1.5*1.5 then
    halt(1);
  v:=-v;
  v:=abs(v);
  if v<>1.5*1.5 then
    halt(1);
  writeln('ok');
end.
