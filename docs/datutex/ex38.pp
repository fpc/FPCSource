Program Example38;

{ This program demonstrates the StartOfADay function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"Start of the day : "dd mmmm yyyy hh:nn:ss';

Var
  Y,M,D : Word;

Begin
  Y:=YearOf(Today);
  M:=MonthOf(Today);
  D:=DayOf(Today);
  Writeln(FormatDateTime(Fmt,StartOfADay(Y,M,D)));
  DecodeDateDay(Today,Y,D);
  Writeln(FormatDateTime(Fmt,StartOfADay(Y,D)));
End.