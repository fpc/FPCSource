uses
  sysutils,dateutils;
var
  date1,
  date2: tdatetime;
  jdate: double;
begin
  date1:=EncodeDateTime(2010,03,22,0,0,0,0);
  date2:=JulianDateToDateTime(2455277.50000);
  if date1<>date2 then
    begin
      writeln(date1:0:12);
      writeln(date2:0:12);
      halt(1);
    end;
  if DateTimeToJulianDate(date2)<>2455277.50000 then
    begin
      writeln(DateTimeToJulianDate(date2):0:5);
      writeln(2455277.50000:0:5);
      halt(2);
    end;
  jdate:=DateTimeToModifiedJulianDate(date1);
  if ModifiedJulianDateToDateTime(jdate)<>date1 then
    begin
      writeln(jdate:0:12);
      writeln(date1:0:12);
      halt(3);
    end;
  date1:=EncodeDateTime(2010,03,23,0,0,0,0);
  date2:=JulianDateToDateTime(2455278.50000);
  if date1<>date2 then
    begin
      writeln(date1:0:12);
      writeln(date2:0:12);
      halt(4);
    end;
  if DateTimeToJulianDate(date2)<>2455278.50000 then
    begin
      writeln(DateTimeToJulianDate(date2):0:5);
      writeln(2455278.50000:0:5);
      halt(5);
    end;
  jdate:=DateTimeToModifiedJulianDate(date1);
  if ModifiedJulianDateToDateTime(jdate)<>date1 then
    begin
      writeln(jdate:0:12);
      writeln(date1:0:12);
      halt(6);
    end;

  date1:=EncodeDateTime(2011,03,26,19,15,30,555);
  if IsInLeapYear(date1) then 
    begin
      writeln('IsInLeapYear test fail');
      halt(7);
    end;
  if not IsPM(date1) then 
    begin
      writeln('IsPM test fail');
      halt(8);
    end;
  if YearOf(date1) <> 2011 then 
    begin
      writeln('YearOf test fail');
      halt(9);
    end;
  if MonthOf(date1) <> 3 then 
    begin
      writeln('MonthOf test fail');
      halt(10);
    end;
  if DayOf(date1) <> 26 then 
    begin
      writeln('DayOf test fail');
      halt(11);
    end;
  if (HourOf(date1) <> 19) or (HourOfTheDay(date1) <> 19) then 
    begin
      writeln('HourOf test fail');
      halt(11);
    end;
  if (MinuteOf(date1) <> 15) or (MinuteOfTheHour(date1) <> 15) then 
    begin
      writeln('MinuteOf test fail');
      halt(12);
    end;
  if (SecondOf(date1) <> 30) or (SecondOfTheMinute(date1) <> 30) then 
    begin
      writeln('SecondOf test fail');
      halt(13);
    end;
  if (MilliSecondOf(date1) <> 555) or (MilliSecondOfTheSecond(date1) <> 555) then 
    begin
      writeln('MilliSecondOf test fail');
      halt(14);
    end;
  if StartOfTheYear(date1) <> EncodeDate(2011, 1, 1) then
    begin
      writeln('StartOfTheYear test fail');
      halt(15);
    end;
  if EndOfTheYear(date1) <> EncodeDateTime(2011,12,31,23,59,59,999) then
    begin
      writeln('EndOfTheYear test fail');
      halt(16);
    end;
  if scandatetime('YYYY.MM.DD HH:NN:SS:ZZZ', '2011.03.29 16:46:56:777') <>
    EncodeDateTime(2011,03,29,16,46,56,777) then
    begin
      writeln('scandatetime test fail');
      halt(17);
    end;
end.
