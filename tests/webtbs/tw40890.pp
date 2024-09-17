{$mode objfpc}
generic procedure Foo<T>;
begin
  WriteLn('Bar');
end;

begin
  try
    specialize Foo<Integer>; // this one works
  except
    specialize Foo<Integer>; // Error: Identifier not found "specialize"
  end;
end.
