Program Example29;

{ This program demonstrates the SetCurrentDir function }

Uses sysutils;

Begin
  If SetCurrentDir ('..') Then
    Writeln ('Now in directory ',GetCurrentDir)
  else
    Writeln ('Change directory to .. failed.');
End.