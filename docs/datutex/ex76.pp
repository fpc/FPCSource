Program Example76;

{ This program demonstrates the IncMinute function }

Uses SysUtils,DateUtils;

Begin
  Writeln('One Minute from now is ',TimeToStr(IncMinute(Time,1)));
  Writeln('One Minute ago from now is ',TimeToStr(IncMinute(Time,-1)));
End.