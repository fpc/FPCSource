{ %OPT=-O3 -Oodeadstore }
procedure p;
  var
    i : longint;
  procedure n;
    begin
      if i<>1234 then
        halt(1);
    end;

  begin
    i:=1234;
    n;
  end;

begin
  p;
  writeln('ok');
end.
