Program Example46;

{ This program demonstrates the MilliSecondOfTheSecond function }

Uses SysUtils,DateUtils;

Var
  N : TDateTime;

Begin
  N:=Now;
  Writeln('MilliSecond of the Second : ',
          MilliSecondOfTheSecond(N));
End.