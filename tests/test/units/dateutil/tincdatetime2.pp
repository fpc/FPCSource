{ IncDay/IncHour/IncMinute/IncSecond/IncMilliSecond with a Double amount, also for dates before 1899-12-30 }
program tincdatetime2;

{$mode objfpc}{$H+}

uses
  SysUtils, Math, DateUtils;

const
  cFmt = 'yyyy-mm-dd hh:nn:ss.zzz';

var
  gErrors: Integer = 0;


procedure Check(const aName: string; aResult: TDateTime; const aExpected: string);

var
  lActual: string;

begin
  lActual:=FormatDateTime(cFmt,aResult);
  if lActual=aExpected then
    Writeln(aName,': ',lActual,' OK')
  else
    begin
    Writeln(aName,': ',lActual,' expected ',aExpected,' ERROR');
    Inc(gErrors);
    end;
end;


procedure CheckSpan(const aName: string; aActual, aExpected: Double);

begin
  if SameValue(aActual,aExpected,1e-3) then
    Writeln(aName,': ',aActual:0:6,' OK')
  else
    begin
    Writeln(aName,': ',aActual:0:6,' expected ',aExpected:0:6,' ERROR');
    Inc(gErrors);
    end;
end;


var
  lPos, lNeg, lNegMs: TDateTime;

begin
  lPos:=EncodeDateTime(2024,1,1,6,0,0,0);
  lNeg:=EncodeDateTime(1899,12,25,6,0,0,0);
  lNegMs:=EncodeDateTime(1899,12,25,6,0,0,100);

  Check('IncDay +1.5', IncDay(lPos,1.5), '2024-01-02 18:00:00.000');
  Check('IncDay -1.5', IncDay(lPos,-1.5), '2023-12-30 18:00:00.000');
  Check('IncDay +0.25 before 1899-12-30', IncDay(lNeg,0.25), '1899-12-25 12:00:00.000');
  Check('IncDay -0.25 before 1899-12-30', IncDay(lNeg,-0.25), '1899-12-25 00:00:00.000');
  Check('IncDay +5.5 to after 1899-12-30', IncDay(lNeg,5.5), '1899-12-30 18:00:00.000');
  Check('IncDay -0.5 to before 1899-12-30', IncDay(EncodeDateTime(1899,12,30,6,0,0,0),-0.5), '1899-12-29 18:00:00.000');

  Check('IncHour +1.5', IncHour(lPos,1.5), '2024-01-01 07:30:00.000');
  Check('IncHour +1.5 before 1899-12-30', IncHour(lNeg,1.5), '1899-12-25 07:30:00.000');
  Check('IncHour -6.5 before 1899-12-30', IncHour(lNeg,-6.5), '1899-12-24 23:30:00.000');
  Check('IncHour -1.5 to before 1899-12-30', IncHour(EncodeDateTime(1899,12,30,1,0,0,0),-1.5), '1899-12-29 23:30:00.000');

  Check('IncMinute +1.5', IncMinute(lPos,1.5), '2024-01-01 06:01:30.000');
  Check('IncMinute +1.5 before 1899-12-30', IncMinute(EncodeDateTime(1899,12,20,23,59,0,0),1.5), '1899-12-21 00:00:30.000');
  Check('IncMinute -0.5 before 1899-12-30', IncMinute(lNeg,-0.5), '1899-12-25 05:59:30.000');

  Check('IncSecond +0.5', IncSecond(lPos,0.5), '2024-01-01 06:00:00.500');
  Check('IncSecond +0.5 before 1899-12-30', IncSecond(lNeg,0.5), '1899-12-25 06:00:00.500');
  Check('IncSecond -1.25 before 1899-12-30', IncSecond(lNeg,-1.25), '1899-12-25 05:59:58.750');

  Check('IncMilliSecond +1500.0', IncMilliSecond(lPos,1500.0), '2024-01-01 06:00:01.500');
  Check('IncMilliSecond -200.0 before 1899-12-30', IncMilliSecond(lNegMs,-200.0), '1899-12-25 05:59:59.900');
  CheckSpan('IncMilliSecond +0.5 span', MilliSecondSpan(lPos,IncMilliSecond(lPos,0.5)), 0.5);
  CheckSpan('IncMilliSecond +0.5 span before 1899-12-30', MilliSecondSpan(lNeg,IncMilliSecond(lNeg,0.5)), 0.5);

  Check('IncDay integer amount', IncDay(lNeg,2), '1899-12-27 06:00:00.000');
  Check('IncHour integer amount', IncHour(lNeg,-7), '1899-12-24 23:00:00.000');

  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
