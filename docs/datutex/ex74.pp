Program Example74;

{ This program demonstrates the IncDay function }

Uses SysUtils,DateUtils;

Begin
  Writeln('One Day from today is ',DateToStr(IncDay(Today,1)));
  Writeln('One Day ago from today is ',DateToStr(IncDay(Today,-1)));
End.