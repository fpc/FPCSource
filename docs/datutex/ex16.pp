Program Example16;

{ This program demonstrates the DaysInMonth function }

Uses SysUtils,DateUtils;

Var
  Y,M : Word;

Begin
  For Y:=1992 to 2010 do
    For M:=1 to 12 do
      Writeln(LongMonthNames[m],' ',Y,' has ',DaysInMonth(EncodeDate(Y,M,1)),' days.');
End.