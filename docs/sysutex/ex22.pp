Program Example22;

{ This program demonstrates the SystemTimeToDateTime function }

Uses sysutils;

Var ST : TSystemTime;

Begin
  DateTimeToSystemTime(Now,ST);
  With St do
    begin
    Writeln ('Today is    ',year,'/',month,'/',Day);
    Writeln ('The time is ',Hour,':',minute,':',Second,'.',MilliSecond);
    end;
  Writeln ('Converted : ',DateTimeToStr(SystemTimeToDateTime(ST)));
End.