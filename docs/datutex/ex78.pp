Program Example78;

{ This program demonstrates the IncMilliSecond function }

Uses SysUtils,DateUtils;

Begin
  Writeln('One MilliSecond from now is ',TimeToStr(IncMilliSecond(Now,1)));
  Writeln('One MilliSecond ago from now is ',TimeToStr(IncMilliSecond(Now,-1)));
End.