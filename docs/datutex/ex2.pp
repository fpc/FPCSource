Program Example2;

{ This program demonstrates the TimeOf function }

Uses SysUtils,DateUtils;

Begin
  Writeln('Time is : ',TimeToStr(TimeOf(Now)));
End.