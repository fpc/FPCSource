Program Example45;

{ This program demonstrates the SecondOfTheMinute function }

Uses SysUtils,DateUtils;

Var
  N : TDateTime;

Begin
  N:=Now;
  Writeln('Second of the Minute      : ',SecondOfTheMinute(N));
  Writeln('MilliSecond of the Minute : ',
          MilliSecondOfTheMinute(N));
End.