Program Example31;

{ This program demonstrates the EndOfAMonth function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"Last day of this month : "dd mmmm yyyy';
Var
  Y,M : Word;

Begin
  Y:=YearOf(Today);
  M:=MonthOf(Today);
  Writeln(FormatDateTime(Fmt,EndOfAMonth(Y,M)));
End.