Program Example35;

{ This program demonstrates the EndOfAWeek function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"Last day of this week : "dd mmmm yyyy hh:nn:ss';
  Fmt2 = '"Last-1 day of this week : "dd mmmm yyyy hh:nn:ss';

Var
  Y,W : Word;

Begin
  Y:=YearOf(Today);
  W:=WeekOf(Today);
  Writeln(FormatDateTime(Fmt,EndOfAWeek(Y,W)));
  Writeln(FormatDateTime(Fmt2,EndOfAWeek(Y,W,6)));
End.