Program Example44;

{ This program demonstrates the MinuteOfTheHour function }

Uses SysUtils,DateUtils;

Var
  N : TDateTime;

Begin
  N:=Now;
  Writeln('Minute of the Hour      : ',MinuteOfTheHour(N));
  Writeln('Second of the Hour      : ',SecondOfTheHour(N));
  Writeln('MilliSecond of the Hour : ',
          MilliSecondOfTheHour(N));
End.