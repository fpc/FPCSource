var
  d: Double;
  c, ce: Currency;
begin
  d := 1;
  c := 2;
  ce := 3;
  c:=c + d;
  writeln('c=', c);
  if c <> ce then begin
    writeln('Invalid currency value. Expected: ', ce);
    halt(1);
  end;
end.
