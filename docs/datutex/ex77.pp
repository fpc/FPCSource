Program Example77;

{ This program demonstrates the IncSecond function }

Uses SysUtils,DateUtils;

Begin
  Writeln('One Second from now is ',TimeToStr(IncSecond(Time,1)));
  Writeln('One Second ago from now is ',TimeToStr(IncSecond(Time,-1)));
End.