Program Example41;

{ This program demonstrates the WeekOfTheMonth function }

Uses SysUtils,DateUtils;

Var
  N : TDateTime;

Begin
  N:=Now;
  Writeln('Week of the Month        : ',WeekOfTheMonth(N));
  Writeln('Day of the Month         : ',DayOfTheMonth(N));
  Writeln('Hour of the Month        : ',HourOfTheMonth(N));
  Writeln('Minute of the Month      : ',MinuteOfTheMonth(N));
  Writeln('Second of the Month      : ',SecondOfTheMonth(N));
  Writeln('MilliSecond of the Month : ',
          MilliSecondOfTheMonth(N));
End.