Program Example23;

{ This program demonstrates the YearOf function }

Uses SysUtils,DateUtils;

Var
  D : TDateTime;

Begin
  D:=Now;
  Writeln('Year        : ',YearOf(D));
  Writeln('Month       : ',MonthOf(D));
  Writeln('Day         : ',DayOf(D));
  Writeln('Week        : ',WeekOf(D));
  Writeln('Hour        : ',HourOf(D));
  Writeln('Minute      : ',MinuteOf(D));
  Writeln('Second      : ',SecondOf(D));
  Writeln('MilliSecond : ',MilliSecondOf(D));
End.