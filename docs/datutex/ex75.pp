Program Example75
;

{ This program demonstrates the IncHour function }

Uses SysUtils,DateUtils;

Begin
  Writeln('One Hour from now is ',DateTimeToStr(IncHour(Now,1)));
  Writeln('One Hour ago from now is ',DateTimeToStr(IncHour(Now,-1)));
End.