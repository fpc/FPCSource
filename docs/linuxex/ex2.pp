Program Example2;

{ Program to demonstrate the GetTimeOfDay function. }

Uses BaseUnix,Unix;

Var TV : TimeVal;
    TZ : TimeZone;

begin
  fpGetTimeOfDay (@TV,@tz);
  Writeln ('Seconds              : ',tv.tv_sec);
  Writeln ('Milliseconds         : ',tv.tv_usec);
  Writeln ('Minutes west of GMT  : ',tz.minuteswest);
  Writeln ('Daylight Saving Time : ',tz.dsttime);
  Writeln ('Seconds in 1 call    : ',fpGetTimeOfDay(nil,nil));
end.
