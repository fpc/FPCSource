procedure test(w : word);forward;

procedure test(a : string);
begin
   Writeln(a);
   test(20);
end;

procedure test(w :word);
begin
   writeln(w);
end;

begin
  test('test');
  test(32);
end.

