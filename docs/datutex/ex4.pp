Program Example4;

{ This program demonstrates the IsPM function }

Uses SysUtils,DateUtils;

Begin
  Writeln('Current time is PM : ',IsPM(Now));
End.