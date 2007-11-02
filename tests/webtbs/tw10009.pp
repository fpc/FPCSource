var
  test : 2..14;

begin
  test:=14;
  case test of
    2..9: halt(1);
    14: ;
  else
    halt(1);
  end;
  writeln('ok');
end.
