Program Example30;

{ This program demonstrates the ChangeFileExt function }

Uses sysutils;

Procedure testit (N,E : String);

begin
  Write ('Changing "',n,'" with extension "',e,'" : ');
  Writeln (ChangeFileExt(N,E));
end;

Begin
  Testit ('report.txt','.pas');
  Testit ('file','.txt');
  Testit ('/path/file.pas','.pp');
  Testit ('/path/file.pp.org','.new');
End.