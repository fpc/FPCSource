Program Example84;

{ This program demonstrates the Trim function }

Uses sysutils;
{$H+}

Procedure Testit (S : String);

begin
  Writeln ('"',Trim(S),'"');
end;

Begin
  Testit ('  ha ha what gets lost ? ');
  Testit (#10#13'haha ');
  Testit ('              ');
End.