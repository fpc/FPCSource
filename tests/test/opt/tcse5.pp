{ %OPT=-Ooautoinline -Oocse }
{$mode objfpc}
type
  trec = record
    b : byte;
  end;

procedure p(var b : byte);
  begin
    b:=b*2;
  end;

var
  rec : trec;
begin
  rec.b:=12;
  p(rec.b);
  if rec.b<>24 then
    halt(1);
  writeln('ok');
end.
