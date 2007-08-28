{ %fail }

procedure t1(l: longint); overload;
begin
  writeln('longint');
end;

procedure t1(l: smallint); overload;
begin
  writeln('smallint');
end;

procedure t1(l: word); overload;
begin
  writeln('word');
end;

var
  c: cardinal;
begin
  c:=1;
  t1(c);
end.
