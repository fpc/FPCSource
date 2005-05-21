var
  C : Char;
  S : AnsiString;
begin
  S := '';
  SetLength(S,1);
  S[1] := '?';
  SetLength(S,2);
  S[2] := '?';
  if (s <> '??') then
    halt(1);

  S := '';
  S := S + '?';
  S := S + '?';
  if (s <> '??') then
    halt(1);
end.
