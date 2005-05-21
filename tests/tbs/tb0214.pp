{ Old file: tbs0253.pp }
{ problem with overloaded procedures and forward       OK 0.99.11 (PFV) }

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
