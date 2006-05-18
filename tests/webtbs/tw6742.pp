type
  openstring=integer;

procedure test(var x:openstring);
begin
  writeln(x);
end;

var
  x: openstring;
begin
  x:=1;
  test(x);
end.
