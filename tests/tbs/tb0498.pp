type
  t1 = longint;

procedure p(t3:word);
var
  t2 : record
    t1 : t1;
end;
begin
  writeln(t3);
end;

begin
  p(10);
end.
