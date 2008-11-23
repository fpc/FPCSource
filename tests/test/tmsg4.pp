{ %opt=-vh -vm05024 -Seh }

procedure test(l: longint);
begin
  writeln('l is unused, but that hint has been suppressed');
end;

begin
  test(5);
end.
