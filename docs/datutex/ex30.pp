Program Example30;

{ This program demonstrates the StartOfAMonth function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"First day of this month : "dd mmmm yyyy';
Var
  Y,M : Word;

Begin
  Y:=YearOf(Today);
  M:=MonthOf(Today);
  Writeln(FormatDateTime(Fmt,StartOfAMonth(Y,M)));
End.