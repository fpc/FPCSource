Program Example40;

{ This program demonstrates the WeekOfTheYear function }

Uses SysUtils,DateUtils;

Var
  N : TDateTime;

Begin
  N:=Now;
  Writeln('Month of the year       : ',MonthOfTheYear(N));
  Writeln('Week of the year        : ',WeekOfTheYear(N));
  Writeln('Day of the year         : ',DayOfTheYear(N));
  Writeln('Hour of the year        : ',HourOfTheYear(N));
  Writeln('Minute of the year      : ',MinuteOfTheYear(N));
  Writeln('Second of the year      : ',SecondOfTheYear(N));
  Writeln('MilliSecond of the year : ',
          MilliSecondOfTheYear(N));
End.