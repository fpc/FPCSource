Program Example15;

{ This program demonstrates the DaysInAYear function }

Uses SysUtils,DateUtils;

Var
  Y : Word;

Begin
  For Y:=1992 to 2010 do
    Writeln(Y,' has ',DaysInAYear(Y),' days.');
End.