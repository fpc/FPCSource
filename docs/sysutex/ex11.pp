Program Example11;

{ This program demonstrates the EncodeDate function }

Uses sysutils;

Var YY,MM,DD : Word;

Begin
  DecodeDate (Date,YY,MM,DD);
  WriteLn ('Today is : ',FormatDateTime ('dd mmmm yyyy',EnCodeDate(YY,Mm,Dd)));
End.