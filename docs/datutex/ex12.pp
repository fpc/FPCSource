Program Example12;

{ This program demonstrates the WeeksInYear function }

Uses SysUtils,DateUtils;

Var
  Y : Word;

Begin
  For Y:=1992 to 2010 do
    Writeln(Y,' has ',WeeksInYear(EncodeDate(Y,2,1)),' weeks.');
End.