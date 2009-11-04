{ %fail }

{$mode tp}
{$p-}

var
  s: string[4];

procedure test(var s: shortstring);
begin
  s:='12345678';
end;

begin
  test(s);
  writeln(s);
end.
