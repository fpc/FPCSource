Program Example71;

{ This program demonstrates the IncYear function }

Uses SysUtils,DateUtils;

Begin
  Writeln('One year from today is ',DateToStr(IncYear(Today,1)));
  Writeln('One year ago from today is ',DateToStr(IncYear(Today,-1)));
End.