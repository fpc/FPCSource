{ %OPT=-gt -O3 }
program project1;

{$mode objfpc}

function CharPos(const S: AnsiString; const C: AnsiChar; const Index: SizeInt): SizeInt;
begin
  for Result := Index to Length(S) do
    if S[Result] = C then
      Exit;
  Result := 0;
end;

begin
  Writeln(CharPos('Hello!', '!', 1));
end.
