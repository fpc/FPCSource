var
  a, b : UnicodeString;
begin
  a := '12345';
  b := Copy(a,1,Length(a));
  if (a <> b) then
    Halt(1);
  WriteLn('ok');
end.
