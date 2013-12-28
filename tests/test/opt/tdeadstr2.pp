{ %OPT=-O3 -Oodeadstore }
var
  p : plongint;

procedure p2;
  var
    i : longint;
  begin
    i:=1234;
    p:=@i;
    if p^<>1234 then
      halt(1);
  end;


begin
  p2;
  writeln('ok');
end.
