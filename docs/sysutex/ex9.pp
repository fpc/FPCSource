Program Example9;

{ This program demonstrates the DecodeDate function }

Uses sysutils;

Var YY,MM,DD : Word;

Begin
  DecodeDate(Date,YY,MM,DD);
  Writeln (Format ('Today is %d/%d/%d',[dd,mm,yy]));
End.