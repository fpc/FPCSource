Program Example38;

{ This program demonstrates the FileExists function }

Uses sysutils;

Begin
  If FileExists(ParamStr(0)) Then
    Writeln ('All is well, I seem to exist.');
End.