{ from GPC test suite }

Program TruncSt3;

Var
  Foo: String [3];

begin
  WriteStr (Foo, 'abcdef');
  if Foo <> 'abc' then
    halt(1);
end.
