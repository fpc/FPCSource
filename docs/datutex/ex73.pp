Program Example73;

{ This program demonstrates the IncWeek function }

Uses SysUtils,DateUtils;

Begin
  Writeln('One Week from today is ',DateToStr(IncWeek(Today,1)));
  Writeln('One Week ago from today is ',DateToStr(IncWeek(Today,-1)));
End.