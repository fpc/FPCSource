program incdatetime_tests;

{$mode objfpc}

uses
  SysUtils, Math, DateUtils;

const
  FMT = 'yyyy-mm-dd hh:nn:ss';

  TDateTimeEpsilon = 2.2204460493e-16;  // Copied from FPC 3.3.1 where it is not public

var
  DateTimeEpsilon_Info: String = '';
  errorcount : Integer = 0;


procedure IncWeek_Test;

  procedure Test(ADateTime: TDateTime; AddWeeks: Integer; Expected: String);
  var
    EndDateTime: TDateTime;
    t1, t2: String;
  begin
    EndDateTime := IncWeek(ADateTime, AddWeeks);
    t1 := FormatDateTime(FMT, ADateTime);
    t2 := FormatDateTime(FMT, EndDateTime);
    Write(t1);
    if AddWeeks > 0 then Write(' + ') else Write(' - ');
    Write(abs(AddWeeks):5, ' weeks --> ', t2, ', Expected: ', Expected);
    Write(' Diff: ', WeeksBetween(ADateTime, EndDateTime):5);
        if t2 = Expected then
      WriteLn(' --> OK')
    else
      begin
        WriteLn(' --> ERROR');
        inc(errorcount);
      end;
  end;

begin
  WriteLn('*** TESTING IncWeek ***', DateTimeEpsilon_Info);
  WriteLn;
  Test(EncodeDateTime(1899,12,31, 0,0,0,0),      1, '1900-01-07 00:00:00');
  Test(EncodeDateTime(1899,12,31, 0,0,0,0),    -52, '1899-01-01 00:00:00');
  Test(EncodeDateTime(1899,12,31, 0,0,0,0), -10000, '1708-05-06 00:00:00');
  Test(EncodeDateTime(1899,12,31, 0,0,0,0),  -9999, '1708-05-13 00:00:00');
  Test(EncodeDateTime(1899,12,31, 0,0,0,0),  -9998, '1708-05-20 00:00:00');

  Test(EncodeDateTime(1899,12,30, 0,0,0,0),      1, '1900-01-06 00:00:00');
  Test(EncodeDateTime(1899,12,30, 0,0,0,0),    -52, '1898-12-31 00:00:00');
  Test(EncodeDateTime(1899,12,30, 0,0,0,0), -10000, '1708-05-05 00:00:00');
  Test(EncodeDateTime(1899,12,30, 0,0,0,0),  -9999, '1708-05-12 00:00:00');
  Test(EncodeDateTime(1899,12,30, 0,0,0,0),  -9998, '1708-05-19 00:00:00');
  Test(EncodeDateTime(1899,12,30, 0,0,0,0),  -1171, '1877-07-21 00:00:00');
  WriteLn('--------------------------------------------------------------------------------');
  WriteLn;
end;

procedure IncDay_Test;

  procedure Test(ADateTime: TDateTime; AddDays: Integer; Expected: String);
  var
    EndDateTime: TDateTime;
    t1, t2: String;
  begin
    EndDateTime := IncDay(ADateTime, AddDays);
    t1 := FormatDateTime(FMT, ADateTime);
    t2 := FormatDateTime(FMT, EndDateTime);
    Write(t1);
    if AddDays > 0 then Write(' + ') else Write(' - ');
    Write(abs(AddDays):4, ' days --> ', t2, ', Expected: ', Expected);
    Write(' Diff: ', DaysBetween(ADateTime, EndDateTime):4);
    if t2 = Expected then
      WriteLn(' --> OK')
    else
      begin
        WriteLn(' --> ERROR');
        inc(errorcount);
      end;
  end;

begin
  WriteLn('*** TESTING IncDay ***', DateTimeEpsilon_Info);
  WriteLn;
  Test(EncodeDateTime(1899,12,31,  0,0,0,0),     1, '1900-01-01 00:00:00');
  Test(EncodeDateTime(1877,07,26,  0,0,0,0),  8193, '1899-12-31 00:00:00');
  Test(EncodeDateTime(1877,07,26,  6,0,0,0),  8193, '1899-12-31 06:00:00');
  WriteLn;
  Test(EncodeDateTime(1899,12,31,  0,0,0,0),    -1, '1899-12-30 00:00:00');
  Test(EncodeDateTime(1899,12,31,  0,0,0,0), -8192, '1877-07-27 00:00:00');
  Test(EncodeDateTime(1899,12,31,  0,0,0,0), -8193, '1877-07-26 00:00:00');
  Test(EncodeDateTime(1899,12,30,  0,0,0,0),    -1, '1899-12-29 00:00:00');
  Test(EncodeDateTime(1899,12,30, 6,30,0,0),    -1, '1899-12-29 06:30:00');
  Test(EncodeDateTime(1899,12,30,  0,0,0,0), -8193, '1877-07-25 00:00:00');
  Test(EncodeDateTime(1899,12,30,  6,0,0,0), -8193, '1877-07-25 06:00:00');
  WriteLn('--------------------------------------------------------------------------------');
  WriteLn;
end;

procedure IncHour_Test;

  procedure Test(ADateTime: TDateTime; AddHours: Integer; Expected: String);
  var
    EndDateTime: TDateTime;
    t1, t2: String;
  begin
    EndDateTime := IncHour(ADateTime, AddHours);
    t1 := FormatDateTime(FMT, ADateTime);
    t2 := FormatDateTime(FMT, EndDateTime);
    Write(t1);
    if AddHours > 0 then Write(' + ') else Write(' - ');
    Write(abs(AddHours):4, ' hrs --> ', t2, ', Expected: ', Expected);
    Write(' Diff: ', HoursBetween(ADateTime, EndDateTime):4);
    if t2 = Expected then
      WriteLn(' --> OK')
    else
      begin
        WriteLn(' --> ERROR');
        inc(errorcount);
      end;
  end;

begin
  WriteLn('*** TESTING IncHour ***', DateTimeEpsilon_Info);
  WriteLn;
  Test(EncodeDateTime(1899,12,31, 23,0,0,0), 1, '1900-01-01 00:00:00');
  Test(EncodeDateTime(1899,12,30, 23,0,0,0), 1, '1899-12-31 00:00:00');
  Test(EncodeDateTime(1899,12,29, 23,0,0,0), 1, '1899-12-30 00:00:00');
  Test(EncodeDateTime(1899,12,28, 23,0,0,0), 1, '1899-12-29 00:00:00');
  Test(EncodeDateTime(1899,12,27, 23,0,0,0), 1, '1899-12-28 00:00:00');
  Test(EncodeDateTime(1899,12,26, 23,0,0,0), 1, '1899-12-27 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,25, 23,0,0,0), 1, '1899-12-26 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,24, 23,0,0,0), 1, '1899-12-25 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,23, 23,0,0,0), 1, '1899-12-24 00:00:00');
  Test(EncodeDateTime(1899,12,22, 23,0,0,0), 1, '1899-12-23 00:00:00');
  Test(EncodeDateTime(1899,12,21, 23,0,0,0), 1, '1899-12-22 00:00:00');
  Test(EncodeDateTime(1899,12,20, 23,5,0,0), 1, '1899-12-21 00:05:00');
  Test(EncodeDateTime(1899,12,29, 15,0,0,0), 9, '1899-12-30 00:00:00');
  Test(EncodeDateTime(1899, 1, 1, 15,0,0,0), 1, '1899-01-01 16:00:00');
  WriteLn;
  Test(EncodeDateTime(1899,12,31,  1,0,0,0),-1, '1899-12-31 00:00:00');
  Test(EncodeDateTime(1899,12,30,  1,0,0,0),-1, '1899-12-30 00:00:00');
  Test(EncodeDateTime(1899,12,29,  1,0,0,0),-1, '1899-12-29 00:00:00');
  Test(EncodeDateTime(1899,12,28,  1,0,0,0),-1, '1899-12-28 00:00:00');
  Test(EncodeDateTime(1899,12,27,  1,0,0,0),-1, '1899-12-27 00:00:00');
  Test(EncodeDateTime(1899,12,26,  1,0,0,0),-1, '1899-12-26 00:00:00');
  Test(EncodeDateTime(1899,12,25,  1,0,0,0),-1, '1899-12-25 00:00:00');
  Test(EncodeDateTime(1899,12,24,  1,0,0,0),-1, '1899-12-24 00:00:00');
  Test(EncodeDateTime(1899,12,23,  1,0,0,0),-1, '1899-12-23 00:00:00');
  Test(EncodeDateTime(1899,12,22,  1,0,0,0),-1, '1899-12-22 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,21,  1,0,0,0),-1, '1899-12-21 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,20,  1,0,0,0),-1, '1899-12-20 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,19,  1,0,0,0),-1, '1899-12-19 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,18,  1,5,0,0),-1, '1899-12-18 00:05:00');   // Error
  WriteLn('--------------------------------------------------------------------------------');
  WriteLn;
end;

procedure IncMinute_Test;

  procedure Test(ADateTime: TDateTime; AddMins: Integer; Expected: String);
  var
    EndDateTime: TDateTime;
    t1, t2: String;
  begin
    EndDateTime := IncMinute(ADateTime, AddMins);
    t1 := FormatDateTime(FMT, ADateTime);
    t2 := FormatDateTime(FMT, EndDateTime);
    Write(t1);
    if AddMins > 0 then Write(' + ') else Write(' - ');
    Write(abs(addMins):4, ' mins --> ', t2, ', Expected: ', Expected);
    Write(' Diff: ', MinutesBetween(ADateTime, EndDateTime):4);
    if t2 = Expected then
      WriteLn(' --> OK')
    else
      begin
        WriteLn(' --> ERROR');
        inc(errorcount);
      end;
  end;

begin
  WriteLn(' *** TESTING IncMinute ***', DateTimeEpsilon_Info);
  WriteLn;
  // Add 20 and 40 minutes -- result must be start of next hour
  Test(EncodeDateTime(1899,12,30, 23,20,0,0), 40, '1899-12-31 00:00:00');

  Test(EncodeDateTime(1899,12,31, 23,0,0,0), 60, '1900-01-01 00:00:00');
  Test(EncodeDateTime(1899,12,30, 23,0,0,0), 60, '1899-12-31 00:00:00');
  Test(EncodeDateTime(1899,12,29, 23,0,0,0), 60, '1899-12-30 00:00:00');
  Test(EncodeDateTime(1899,12,28, 23,0,0,0), 60, '1899-12-29 00:00:00');
  Test(EncodeDateTime(1899,12,27, 23,0,0,0), 60, '1899-12-28 00:00:00');
  Test(EncodeDateTime(1899,12,26, 23,0,0,0), 60, '1899-12-27 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,25, 23,0,0,0), 60, '1899-12-26 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,24, 23,0,0,0), 60, '1899-12-25 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,23, 23,0,0,0), 60, '1899-12-24 00:00:00');
  Test(EncodeDateTime(1899,12,22, 23,0,0,0), 60, '1899-12-23 00:00:00');
  Test(EncodeDateTime(1899,12,21, 23,0,0,0), 60, '1899-12-22 00:00:00');
  Test(EncodeDateTime(1899,12,20, 23,5,1,0), 60, '1899-12-21 00:05:01');
  Test(EncodeDateTime(1898,12,30,  1,20,0,0),40, '1898-12-30 02:00:00');
  WriteLn;
  Test(EncodeDateTime(1899,12,31,  1,0,0,0),-60, '1899-12-31 00:00:00');
  Test(EncodeDateTime(1899,12,30,  1,0,0,0),-60, '1899-12-30 00:00:00');
  Test(EncodeDateTime(1899,12,29,  1,0,0,0),-60, '1899-12-29 00:00:00');
  Test(EncodeDateTime(1899,12,28,  1,0,0,0),-60, '1899-12-28 00:00:00');
  Test(EncodeDateTime(1899,12,27,  1,0,0,0),-60, '1899-12-27 00:00:00');
  Test(EncodeDateTime(1899,12,26,  1,0,0,0),-60, '1899-12-26 00:00:00');
  Test(EncodeDateTime(1899,12,25,  1,0,0,0),-60, '1899-12-25 00:00:00');
  Test(EncodeDateTime(1899,12,24,  1,0,0,0),-60, '1899-12-24 00:00:00');
  Test(EncodeDateTime(1899,12,23,  1,0,0,0),-60, '1899-12-23 00:00:00');
  Test(EncodeDateTime(1899,12,22,  1,0,0,0),-60, '1899-12-22 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,21,  1,0,0,0),-60, '1899-12-21 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,20,  1,0,0,0),-60, '1899-12-20 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,19,  1,0,0,0),-60, '1899-12-19 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,18,  1,5,1,0),-60, '1899-12-18 00:05:01');   // Error
  WriteLn('--------------------------------------------------------------------------------');
  WriteLn;
end;

procedure IncSecond_Test;

  procedure Test(ADateTime: TDateTime; AddSeconds: Integer; Expected: String);
  var
    EndDateTime: TDateTime;
    t1, t2: String;
  begin
    EndDateTime := IncSecond(ADateTime, AddSeconds);
    t1 := FormatDateTime(FMT, ADateTime);
    t2 := FormatDateTime(FMT, EndDateTime);
    Write(t1);
    if AddSeconds > 0 then Write(' + ') else Write(' - ');
    Write(abs(AddSeconds):5, ' secs --> ', t2, ', Expected: ', Expected);
    Write(' Diff: ', SecondsBetween(ADateTime, EndDateTime):5);
    if t2 = Expected then
      WriteLn(' --> OK')
    else
      begin
        WriteLn(' --> ERROR');
        inc(errorcount);
      end;
  end;

begin
  WriteLn(' *** TESTING IncSecond ***', DateTimeEpsilon_Info);
  WriteLn;
  Test(EncodeDateTime(1899,12,31, 23,0,0,0), 3600, '1900-01-01 00:00:00');   // 3600 sec = 1 hour
  Test(EncodeDateTime(1899,12,30, 23,0,0,0), 3600, '1899-12-31 00:00:00');
  Test(EncodeDateTime(1899,12,29, 23,0,0,0), 3600, '1899-12-30 00:00:00');
  Test(EncodeDateTime(1899,12,28, 23,0,0,0), 3600, '1899-12-29 00:00:00');
  Test(EncodeDateTime(1899,12,27, 23,0,0,0), 3600, '1899-12-28 00:00:00');
  Test(EncodeDateTime(1899,12,26, 23,0,0,0), 3600, '1899-12-27 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,25, 23,0,0,0), 3600, '1899-12-26 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,24, 23,0,0,0), 3600, '1899-12-25 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,23, 23,0,0,0), 3600, '1899-12-24 00:00:00');
  Test(EncodeDateTime(1899,12,22, 23,0,0,0), 3600, '1899-12-23 00:00:00');
  Test(EncodeDateTime(1899,12,21, 23,0,0,0), 3600, '1899-12-22 00:00:00');
  Test(EncodeDateTime(1899,12,20, 23,5,1,0), 3600, '1899-12-21 00:05:01');
  Test(EncodeDateTime(1899,12,20, 23,5,1,0),   59, '1899-12-20 23:06:00');
  Test(EncodeDateTime(1899,12,29, 23,59,42,0), 18, '1899-12-30 00:00:00');
  Test(EncodeDateTime(1899,12,30, 23,59,33,0), 27, '1899-12-31 00:00:00');
  WriteLn;
  Test(EncodeDateTime(1899,12,31,  1,0,0,0),-3600, '1899-12-31 00:00:00');
  Test(EncodeDateTime(1899,12,30,  1,0,0,0),-3600, '1899-12-30 00:00:00');
  Test(EncodeDateTime(1899,12,29,  1,0,0,0),-3600, '1899-12-29 00:00:00');
  Test(EncodeDateTime(1899,12,28,  1,0,0,0),-3600, '1899-12-28 00:00:00');
  Test(EncodeDateTime(1899,12,27,  1,0,0,0),-3600, '1899-12-27 00:00:00');
  Test(EncodeDateTime(1899,12,26,  1,0,0,0),-3600, '1899-12-26 00:00:00');
  Test(EncodeDateTime(1899,12,25,  1,0,0,0),-3600, '1899-12-25 00:00:00');
  Test(EncodeDateTime(1899,12,24,  1,0,0,0),-3600, '1899-12-24 00:00:00');
  Test(EncodeDateTime(1899,12,23,  1,0,0,0),-3600, '1899-12-23 00:00:00');
  Test(EncodeDateTime(1899,12,22,  1,0,0,0),-3600, '1899-12-22 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,21,  1,0,0,0),-3600, '1899-12-21 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,20,  1,0,0,0),-3600, '1899-12-20 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,19,  1,0,0,0),-3600, '1899-12-19 00:00:00');   // Error
  Test(EncodeDateTime(1899,12,18,  1,5,1,0),-3600, '1899-12-18 00:05:01');   // Error
  WriteLn('--------------------------------------------------------------------------------');
  WriteLn;
end;

procedure IncMilliSecond_Test;

  procedure Test(ADateTime: TDateTime; AddMilliSeconds: Integer; Expected: String);
  var
    EndDateTime: TDateTime;
    t1, t2: String;
  begin
    EndDateTime := IncMilliSecond(ADateTime, AddMilliSeconds);
    t1 := FormatDateTime(FMT+'.zzz', ADateTime);
    t2 := FormatDateTime(FMT+'.zzz', EndDateTime);
    Write(t1);
    if AddMilliSeconds > 0 then Write(' + ') else Write(' - ');
    Write(abs(AddMilliSeconds):7, ' ms --> ', t2, ', Expected: ', Expected);
    Write(' Diff: ', MilliSecondsBetween(ADateTime, EndDateTime):7);
    if t2 = Expected then
      WriteLn(' --> OK')
    else
      begin
        WriteLn(' --> ERROR');
        inc(errorcount);
      end;
  end;

begin
  WriteLn(' *** TESTING IncMilliSecond ***', DateTimeEpsilon_Info);
  WriteLn;
  Test(EncodeDateTime(1899,12,29, 23,59,59,999),   1, '1899-12-30 00:00:00.000');
  Test(EncodeDateTime(1899,12,30, 23,59,59,999),   1, '1899-12-31 00:00:00.000');
  Test(EncodeDateTime(1899,12,31, 23,59,59,999),   1, '1900-01-01 00:00:00.000');
  Test(EncodeDateTime(1899,12,31, 23,0,0,0), 3600000, '1900-01-01 00:00:00.000');   // 3600000 ms = 1 hour
  Test(EncodeDateTime(1899,12,30, 23,0,0,0), 3600000, '1899-12-31 00:00:00.000');
  Test(EncodeDateTime(1899,12,29, 23,0,0,0), 3600000, '1899-12-30 00:00:00.000');
  Test(EncodeDateTime(1899,12,28, 23,0,0,0), 3600000, '1899-12-29 00:00:00.000');
  Test(EncodeDateTime(1899,12,27, 23,0,0,0), 3600000, '1899-12-28 00:00:00.000');
  Test(EncodeDateTime(1899,12,26, 23,0,0,0), 3600000, '1899-12-27 00:00:00.000');   // Error
  Test(EncodeDateTime(1899,12,25, 23,0,0,0), 3600000, '1899-12-26 00:00:00.000');   // Error
  Test(EncodeDateTime(1899,12,24, 23,0,0,0), 3600000, '1899-12-25 00:00:00.000');   // Error
  Test(EncodeDateTime(1899,12,23, 23,0,0,0), 3600000, '1899-12-24 00:00:00.000');
  Test(EncodeDateTime(1899,12,22, 23,0,0,0), 3600000, '1899-12-23 00:00:00.000');
  Test(EncodeDateTime(1899,12,21, 23,0,0,0), 3600000, '1899-12-22 00:00:00.000');
  Test(EncodeDateTime(1899,12,20,  1,0,0,700),   300, '1899-12-20 01:00:01.000');
  Test(EncodeDateTime(1900,1, 1,  23,59,59,100), 900, '1900-01-02 00:00:00.000');
  WriteLn;
  Test(EncodeDateTime(1899,12,31,  0,0,0,0),      -1, '1899-12-30 23:59:59.999');
  Test(EncodeDateTime(1899,12,30,  0,0,0,0),      -1, '1899-12-29 23:59:59.999');
  Test(EncodeDateTime(1899,12,29,  0,0,0,0),      -1, '1899-12-28 23:59:59.999');
  Test(EncodeDateTime(1899,12,31,  1,0,0,0),-3600000, '1899-12-31 00:00:00.000');
  Test(EncodeDateTime(1899,12,30,  1,0,0,0),-3600000, '1899-12-30 00:00:00.000');
  Test(EncodeDateTime(1899,12,29,  1,0,0,0),-3600000, '1899-12-29 00:00:00.000');
  Test(EncodeDateTime(1899,12,28,  1,0,0,0),-3600000, '1899-12-28 00:00:00.000');
  Test(EncodeDateTime(1899,12,27,  1,0,0,0),-3600000, '1899-12-27 00:00:00.000');
  Test(EncodeDateTime(1899,12,26,  1,0,0,0),-3600000, '1899-12-26 00:00:00.000');
  Test(EncodeDateTime(1899,12,25,  1,0,0,0),-3600000, '1899-12-25 00:00:00.000');
  Test(EncodeDateTime(1899,12,24,  1,0,0,0),-3600000, '1899-12-24 00:00:00.000');
  Test(EncodeDateTime(1899,12,23,  1,0,0,0),-3600000, '1899-12-23 00:00:00.000');
  Test(EncodeDateTime(1899,12,22,  1,0,0,0),-3600000, '1899-12-22 00:00:00.000');   // Error
  Test(EncodeDateTime(1899,12,21,  1,0,0,0),-3600000, '1899-12-21 00:00:00.000');   // Error
  Test(EncodeDateTime(1899,12,20,  1,0,0,0),-3600000, '1899-12-20 00:00:00.000');   // Error
  Test(EncodeDateTime(1899,12,19,  1,0,0,0),-3600000, '1899-12-19 00:00:00.000');   // Error
  Test(EncodeDateTime(1899,12,30,  0,0,0,0),     -30, '1899-12-29 23:59:59.970');
  Test(EncodeDateTime(1899,12,30,  0,0,1,29),    -30, '1899-12-30 00:00:00.999');
  WriteLn('--------------------------------------------------------------------------------');
  WriteLn;
end;

begin
  Str(TDateTimeEpsilon, DateTimeEpsilon_Info);
  DateTimeEpsilon_Info := ' (DateTimeEpsilon=' + DateTimeEpsilon_Info + ')';

  IncWeek_Test;
  IncDay_Test;
  IncHour_Test;
  IncMinute_Test;
  IncSecond_Test;
  IncMillisecond_Test;
  WriteLn(errorcount,' errors encountered');
  Halt(errorcount);
end.
