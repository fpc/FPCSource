Program Example32;

{ This program demonstrates the DoDirSeparators function }
{$H+}

Uses sysutils;

Procedure Testit (F : String);

begin
  Writeln ('Before : ',F);
  DoDirSeparators (F);
  Writeln ('After  : ',F);
end;

Begin
  Testit (GetCurrentDir);
  Testit ('c:\pp\bin\win32');
  Testit ('/usr/lib/fpc');
  Testit ('\usr\lib\fpc');
End.