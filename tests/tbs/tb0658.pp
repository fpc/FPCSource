{$r+,q+}

procedure test(i: int64);
begin
  if (i>0) and (i<$1fff) then
    halt(1);
end;

begin
  test(0);
end.
