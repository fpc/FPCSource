Program Example39;

{ This program demonstrates the EndOfADay function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"End of the day : "dd mmmm yyyy hh:nn:ss';

Var
  Y,M,D : Word;

Begin
  Y:=YearOf(Today);
  M:=MonthOf(Today);
  D:=DayOf(Today);
  Writeln(FormatDateTime(Fmt,EndOfADay(Y,M,D)));
  DecodeDateDay(Today,Y,D);
  Writeln(FormatDateTime(Fmt,EndOfADay(Y,D)));
End.