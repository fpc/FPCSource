Program Example2;

{ This program demonstrates the DateTimeToFileDate function }

Uses sysutils;

Begin
  Writeln ('FileTime of now would be: ',DateTimeToFileDate (Now));
End.