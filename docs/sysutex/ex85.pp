Program Example85;

{ This program demonstrates the TrimLeft function }

Uses sysutils;
{$H+}

Procedure Testit (S : String);

begin
  Writeln ('"',TrimLeft(S),'"');
end;

Begin
  Testit ('  ha ha what gets lost ? ');
  Testit (#10#13'haha ');
  Testit ('              ');
End.