{ TryISO8601ToDate with the time zone offset of the string }
program tiso8601offset;

{$mode objfpc}{$H+}

uses
  SysUtils, Math, DateUtils;

const
  cFmt = 'yyyy-mm-dd hh:nn:ss';

var
  gErrors: Integer = 0;


procedure Fail(const aMsg: string);

begin
  Writeln(aMsg,' ERROR');
  Inc(gErrors);
end;


procedure Test(const aISO: string; const aExpectedUTC: string; aExpectedOffset: Double);

var
  lDate, lPlain: TDateTime;
  lOffset: Double;

begin
  if not TryISO8601ToDate(aISO,lDate,lOffset) then
    begin
    Fail(aISO+': not accepted');
    exit;
    end;
  if FormatDateTime(cFmt,lDate)<>aExpectedUTC then
    Fail(aISO+': UTC '+FormatDateTime(cFmt,lDate)+' expected '+aExpectedUTC)
  else if not SameValue(lOffset,aExpectedOffset) then
    Fail(aISO+': offset '+FloatToStr(lOffset)+' expected '+FloatToStr(aExpectedOffset))
  else
    Writeln(aISO,': ',FormatDateTime(cFmt,lDate),' offset ',lOffset:0:2,' OK');
  if not TryISO8601ToDate(aISO,lPlain) or (lPlain<>lDate) then
    Fail(aISO+': overload without offset gives a different result');
  if not TryISO8601ToDate(aISO,lDate,lOffset,False) or not SameValue(lOffset,aExpectedOffset) then
    Fail(aISO+': offset with ReturnUTC=False');
end;


procedure TestInvalid(const aISO: string);

var
  lDate: TDateTime;
  lOffset: Double;

begin
  lOffset:=99;
  if TryISO8601ToDate(aISO,lDate,lOffset) then
    Fail(aISO+': accepted')
  else if lOffset<>0 then
    Fail(aISO+': offset not 0 on failure')
  else
    Writeln(aISO,': rejected OK');
end;


begin
  Test('2024-01-01T12:34:56+01:00', '2024-01-01 11:34:56', 1);
  Test('2024-01-01T12:34:56-05:30', '2024-01-01 18:04:56', -5.5);
  Test('2024-01-01T12:34:56+0100', '2024-01-01 11:34:56', 1);
  Test('2024-01-01T12:34:56+01', '2024-01-01 11:34:56', 1);
  Test('2024-01-01T12:34:56.5+05:45', '2024-01-01 06:49:56', 5.75);
  Test('2024-01-01T00:30:00+02:00', '2023-12-31 22:30:00', 2);
  Test('2024-01-01T23:30:00-02:00', '2024-01-02 01:30:00', -2);
  Test('2024-01-01T12:34:56Z', '2024-01-01 12:34:56', 0);
  Test('2024-01-01T12:34:56', '2024-01-01 12:34:56', 0);
  Test('2024-01-01', '2024-01-01 00:00:00', 0);
  Test('T12:00:00+02:00', '1899-12-30 10:00:00', 2);
  TestInvalid('');
  TestInvalid('2024-01-01T12:34:56+1:00');
  TestInvalid('not a date');
  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
