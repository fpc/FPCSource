Program Example34;

{ This program demonstrates the ExtractFileName function }
{$H+}
Uses sysutils;

Procedure Testit(F : String);

begin
 Writeln ('FileName      : ',F);
 Writeln ('Has Name      : ',ExtractFileName(F));
 Writeln ('Has Path      : ',ExtractFilePath(F));
 Writeln ('Has Extension : ',ExtractFileExt(F));
 Writeln ('Has Directory : ',ExtractFileDir(F));
 Writeln ('Has Drive     : ',ExtractFileDrive(F));
end;

Begin
  Testit (Paramstr(0));
  Testit ('/usr/local/bin/mysqld');
  Testit ('c:\pp\bin\win32\ppc386.exe');
  Testit ('/pp/bin/win32/ppc386.exe');
End.