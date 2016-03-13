{ %OPT=-Oodfa -vwh -Seh }
procedure modify(var p: pointer);
begin
  inc(p);
end;

procedure test(p: pointer);
begin
  modify(p);
end;

begin
  test(nil);
end.
