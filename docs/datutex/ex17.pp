Program Example17;

{ This program demonstrates the DaysInAMonth function }

Uses SysUtils,DateUtils;

Var
  Y,M : Word;

Begin
  For Y:=1992 to 2010 do
    For M:=1 to 12 do
      Writeln(LongMonthNames[m],' ',Y,' has ',DaysInAMonth(Y,M),' days.');
End.