Program Example12;

{ This program demonstrates the EncodeTime function }

Uses sysutils;

Var Hh,MM,SS,MS : Word;

Begin
  DeCodeTime (Time,Hh,MM,SS,MS);
  Writeln ('Present Time is : ',FormatDateTime('hh:mm:ss',EnCodeTime (HH,MM,SS,MS)));
End.