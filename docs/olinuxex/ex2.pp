Program Example2;

{ Program to demonstrate the GetTimeOfDay function. }

Uses oldlinux;

Var TV : TimeVal;
    TZ : TimeZone;

begin
  GetTimeOfDay (TV);
  Writeln ('Seconds              : ',tv.sec);
  Writeln ('Milliseconds         : ',tv.usec);
  Writeln ('Minutes west of GMT  : ',tz.minuteswest);
  Writeln ('Daylight Saving Time : ',tz.dsttime);
  Writeln ('Seconds in 1 call    : ',GetTimeOfDay);
end.
