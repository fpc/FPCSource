Program Example72;

{ This program demonstrates the IncMonth function }

Uses SysUtils,DateUtils;

Begin
  Writeln('One Month from today is ',DateToStr(IncMonth(Today,1)));
  Writeln('One Month ago from today is ',DateToStr(IncMonth(Today,-1)));
End.