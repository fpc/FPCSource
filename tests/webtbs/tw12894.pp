program Project1;

uses
 Classes, SysUtils, DateUtils;

var
 utime : longword;
 sec : word;
 currentDt, convertedDt : TDateTime;
 times: longint;
 s1, s2: ansistring;

begin
 for sec := 0 to 59 do
   begin
     currentDt := EncodeDateTime(1989, 9, 16, 12, 0, sec, 0);
     utime := DateTimeToUnix(currentDt);
     convertedDt := UnixToDateTime(utime);
     s1:=FormatDateTime('mm/dd/yyyy HH:nn:ss', currentDt);
     s2:=FormatDateTime('mm/dd/yyyy HH:nn:ss', convertedDt);
     writeln(s1 + ' = ' + IntToStr(utime) + ' = ' + s2);
     if (s1<>s2) then
       halt(1);
 end;
 for times:=-10000 to 10000 do
   if times<>datetimetounix(unixtodatetime(times)) then
     begin
       writeln('error for ',times,', becomes ',datetimetounix(unixtodatetime(times)));
       halt(2);
     end;

 // check some borderline cases
 currentDt := EncodeDateTime(1899, 12, 29, 6, 0, 0, 0);
 convertedDt := EncodeDateTime(1899, 12, 30, 6, 0, 0, 0);
 writeln(currentDt:0:4,' - ',convertedDt:0:4);
 s1:=FormatDateTime('mm/dd/yyyy HH:nn:ss', currentDt);
 s2:=FormatDateTime('mm/dd/yyyy HH:nn:ss', convertedDt);
 writeln(s1);
 writeln(s2);
 if (currentDt<>-1.25) or
    (convertedDt<>0.25) or
    (s1<>'12-29-1899 06:00:00') or
    (s2<>'12-30-1899 06:00:00') or
    (DaysBetween(currentDt,convertedDt)<>1) or
    (HoursBetween(currentDt,convertedDt)<>24) or
    (MinutesBetween(currentDt,convertedDt)<>24*60) or
    (SecondsBetween(currentDt,convertedDt)<>24*60*60) or
    (MilliSecondsBetween(currentDt,convertedDt)<>24*60*60*1000) then
   begin
     writeln('between ',s1,' and ',s2);
     writeln(DaysBetween(currentDt,convertedDt));
     writeln(HoursBetween(currentDt,convertedDt));
     writeln(MinutesBetween(currentDt,convertedDt));
     writeln(SecondsBetween(currentDt,convertedDt));
     writeln(MilliSecondsBetween(currentDt,convertedDt));
     halt(3);
   end;
 currentDt := EncodeDateTime(1899, 12, 30, 6, 0, 0, 0);
 convertedDt := EncodeDateTime(1899, 12, 29, 6, 0, 0, 0);
 if (DaysBetween(currentDt,convertedDt)<>1) or
    (HoursBetween(currentDt,convertedDt)<>24) or
    (MinutesBetween(currentDt,convertedDt)<>24*60) or
    (SecondsBetween(currentDt,convertedDt)<>24*60*60) or
    (MilliSecondsBetween(currentDt,convertedDt)<>24*60*60*1000) then
   halt(4);
 currentDt := EncodeDateTime(1898, 12, 30, 6, 0, 0, 0);
 convertedDt := EncodeDateTime(1899, 12, 30, 6, 0, 0, 0);
 { 0 and 11 rather than 1 and 12, because YearsBetween and MonthsBetween
   are averaged over 4 years -> include a leap year }
 if (YearsBetween(currentDt,convertedDt)<>0) or
    (MonthsBetween(currentDt,convertedDt)<>11) or
    (DaysBetween(currentDt,convertedDt)<>365) or
    (HoursBetween(currentDt,convertedDt)<>365*24) or
    (MinutesBetween(currentDt,convertedDt)<>365*24*60) or
    (SecondsBetween(currentDt,convertedDt)<>365*24*60*60) or
    (MilliSecondsBetween(currentDt,convertedDt)<>365*24*60*60*1000) then
   begin
     writeln('between ',s1,' and ',s2);
     writeln(YearsBetween(currentDt,convertedDt));
     writeln(MonthsBetween(currentDt,convertedDt));
     writeln(DaysBetween(currentDt,convertedDt));
     writeln(HoursBetween(currentDt,convertedDt));
     writeln(MinutesBetween(currentDt,convertedDt));
     writeln(SecondsBetween(currentDt,convertedDt));
     writeln(MilliSecondsBetween(currentDt,convertedDt));
     halt(5);
   end;
 currentDt := EncodeDateTime(1898, 12, 29, 6, 0, 0, 0);
 convertedDt := EncodeDateTime(1899, 12, 30, 6, 0, 0, 0);
 if (YearsBetween(currentDt,convertedDt)<>1) or
    (MonthsBetween(currentDt,convertedDt)<>12) or
    (DaysBetween(currentDt,convertedDt)<>366) or
    (HoursBetween(currentDt,convertedDt)<>366*24) or
    (MinutesBetween(currentDt,convertedDt)<>366*24*60) or
    (SecondsBetween(currentDt,convertedDt)<>366*24*60*60) or
    (MilliSecondsBetween(currentDt,convertedDt)<>366*24*60*60*1000) then
   halt(6);
  currentDt := 39939.796069305557;
  convertedDt := 39939.0;
  if YearsBetween(currentDt,convertedDt)<>0 then
    halt(7);
// convertedDt:=incseconds(currentDt,

end.
