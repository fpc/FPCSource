{ %fail }
program tConstRef2;

procedure TestConstRef(constref AParam: integer);
begin
  AParam := 5;
end;

begin
  TestConstRef(1);
end.

