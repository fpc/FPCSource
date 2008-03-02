{ %fail }
procedure p1(const b;l:longint);
begin
end;

begin
  // Expected error: variable required
  p1(1,sizeof(1));
end.
