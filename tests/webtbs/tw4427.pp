{$inline on}

procedure test(p: pchar);
begin
  if pchar(p)^ <> 'a' then
    halt(1);
end;

procedure test(const p); inline;
begin
  test(pchar(@p));
end;

begin
  test('abc');
end.
