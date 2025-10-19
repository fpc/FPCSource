{ %OPT=-O3 -Sg }
program app;

procedure kek(value: UInt32);
label
  mylabel;
begin
  while value shr 24 = 0 do
  begin
mylabel:
  end;

  if Random>0.0 then
  begin
    goto mylabel;
  end;
end;

begin
end.
