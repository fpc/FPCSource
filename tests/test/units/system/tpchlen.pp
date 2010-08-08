procedure test(p: pchar; len: longint);
begin
  if (length(p)<>len) then
    halt(1);
end;

begin
  test(nil,0);
  test(#0,0);
  test('a',1);
  test('hello',5);
end.
