program tw32034;

{$H-}

var
  s: String;
begin
  for s in (['1char','2chars']) do begin
    WriteLn(s);
    if (s <> '1char') and (s <> '2chars') then
      Halt(1);
  end;
end.

