{ %FAIL }

program tb0295;

{$V+}
{$P+}

procedure Test(var aArg: ShortString);
begin
end;

{$P-}

procedure Test2(var aArg: ShortString);
begin
end;

var
  s: String[5];
begin
  s := 'Foo';
  Test2(s);
end.
