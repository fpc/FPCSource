Program Example39;

{ This program demonstrates the FileGetDate function }

Uses sysutils;

Var F,D : Longint;

Begin
  F:=FileCreate('test.dat');
  D:=FileGetDate(F);
  Writeln ('File created on ',DateTimeToStr(FileDateToDateTime(D)));
  FileClose(F);
  DeleteFile('test.dat');
End.