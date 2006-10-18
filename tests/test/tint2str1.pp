{$mode macpas}

procedure test(s: string);
begin
  if s <> 'abcd' then
    halt(1);
end;

var
  l: longint;
begin
  l := 'abcd';
  test(l);
end.
