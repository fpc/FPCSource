Program Example42;

{ This program demonstrates the WeekOfTheMonth function }

Uses SysUtils,DateUtils;

Var
  N : TDateTime;

Begin
  N:=Now;
  Writeln('Day of the Week         : ',DayOfTheWeek(N));
  Writeln('Hour of the Week        : ',HourOfTheWeek(N));
  Writeln('Minute of the Week      : ',MinuteOfTheWeek(N));
  Writeln('Second of the Week      : ',SecondOfTheWeek(N));
  Writeln('MilliSecond of the Week : ',
          MilliSecondOfTheWeek(N));
End.