Program Example86;

{ This program demonstrates the TrimRight function }

Uses sysutils;
{$H+}

Procedure Testit (S : String);

begin
  Writeln ('"',TrimRight(S),'"');
end;

Begin
  Testit ('  ha ha what gets lost ? ');
  Testit (#10#13'haha ');
  Testit ('              ');
End.