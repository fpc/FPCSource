Program Example1;

{ This program demonstrates the DateOf function }

Uses SysUtils,DateUtils;

Begin
  Writeln('Date is: ',DateTimeToStr(DateOf(Now)));
End.