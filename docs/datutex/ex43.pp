Program Example43;

{ This program demonstrates the HourOfTheDay function }

Uses SysUtils,DateUtils;

Var
  N : TDateTime;

Begin
  N:=Now;
  Writeln('Hour of the Day        : ',HourOfTheDay(N));
  Writeln('Minute of the Day      : ',MinuteOfTheDay(N));
  Writeln('Second of the Day      : ',SecondOfTheDay(N));
  Writeln('MilliSecond of the Day : ',
          MilliSecondOfTheDay(N));
End.