{$mode macpas}

procedure test;
var
  d: dword;
begin
  d:=(65 shl 24) or (66 shl 16) or (67 shl 8) or 68;
  if (d<>'ABCD') then
    halt(1);
end;

begin
  test;
end.
