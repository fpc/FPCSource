Program Example14;

{ This program demonstrates the DaysInYear function }

Uses SysUtils,DateUtils;

Var
  Y : Word;

Begin
  For Y:=1992 to 2010 do
    Writeln(Y,' has ',DaysInYear(EncodeDate(Y,1,1)),' days.');
End.