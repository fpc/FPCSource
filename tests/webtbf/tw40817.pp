{ %fail }
procedure proc;
begin
end;

procedure test(const o);
begin
end;

begin
  test(proc); // project1.lpr(10,3) Error: Internal error 2011010304
end.
