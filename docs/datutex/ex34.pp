Program Example34;

{ This program demonstrates the StartOfAWeek function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"First day of this week : "dd mmmm yyyy hh:nn:ss';
  Fmt2 = '"Second day of this week : "dd mmmm yyyy hh:nn:ss';

Var
  Y,W : Word;

Begin
  Y:=YearOf(Today);
  W:=WeekOf(Today);
  Writeln(FormatDateTime(Fmt,StartOfAWeek(Y,W)));
  Writeln(FormatDateTime(Fmt2,StartOfAWeek(Y,W,2)));
End.