program test;

{$MODE OBJFPC}
{$MODESWITCH UNICODESTRINGS}

var
  S: string;
  pS: PChar;
  ppS: PPChar;

begin
  S := 'test string';
  pS := @S[1];
  ppS := @pS;
  pS := ppS^;  // Error: Incompatible types: got "PChar" expected "PWideChar"
  WriteLn(string(pS));
end.
