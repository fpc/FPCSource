{ %fail }

{$mode fpc}

procedure test(s: string);
begin
  halt(0);
end;

var
  l: longint;
begin
  l := 'abcd';
  test(l);
end.
