Program Example5;

{ This program demonstrates the DateTimeToSystemTime function }

Uses sysutils;

Var ST : TSystemTime;

Begin
  DateTimeToSystemTime(Now,ST);
  With St do
    begin
    Writeln ('Today is    ',year,'/',month,'/',Day);
    Writeln ('The time is ',Hour,':',minute,':',Second,'.',MilliSecond);
    end;
End.