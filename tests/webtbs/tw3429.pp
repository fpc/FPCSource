uses uw3429;

var
  ok : byte;
begin
  if v=7 then
    begin
      ok:=ok or 1;
      writeln('ok1');
    end;
  if v='7' then
    begin
      ok:=ok or 2;
      writeln('ok2');
    end;
  writeln(v);
  if ok<>3 then
    halt(1);
end.
