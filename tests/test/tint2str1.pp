{$mode macpas}

procedure test(const s: string);
begin
  if s <> 'abcd' then
    halt(1);
end;

procedure test2(c1,c2,c3,c4: char);
begin
  if (c1 <> 'a') or (c2 <> 'b') or (c3 <> 'c') or (c4 <> 'd') then
    halt(2);
end;


var
  l: longint;
begin
  l := 'abcd';
  test(l);
  test2(char(l shr 24),char(l shr 16),char(l shr 8),char(l));
end.
