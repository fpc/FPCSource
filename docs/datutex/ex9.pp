Program Example9;

{ This program demonstrates the IsValidDateDay function }

Uses SysUtils,DateUtils;

Var
  Y : Word;

Begin
  For Y:=1996 to 2004 do
    if  IsValidDateDay(Y,366) then
      Writeln(Y,' is a leap year');
End.