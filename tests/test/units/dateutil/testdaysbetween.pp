program TestProject;

{$mode objfpc}
uses
  SysUtils, Math, DateUtils;

type
  TBetweenFunc = function (const dt1, dt2: TDateTime): Int64;

// Re-implement here because we need an Int64 result.
function DaysBetween(const dt1, dt2: TDateTime): Int64;
begin
  Result := DateUtils.DaysBetween(dt1, dt2);
end;

// Performs the test
procedure Test(Caption: String; BetweenFunc: TBetweenFunc; dt1, dt2: TDateTime;
  Expected: Integer);
var
  res: Integer;
begin
  // Run the provided *Between function
  res := BetweenFunc(dt1, dt2);
  Write(Format('    %s(%s [%.6f], %s [%.6f]) = %d, expected: %d', [
    Caption, DateTimeToStr(dt1), dt1, dateTimeToStr(dt2), dt2, res, expected]));

  // Compare result with expected value
  if res = expected then
    WriteLn
  else
   begin
    WriteLn(' ---> ERROR');
    exitcode:=1;
   end;
end;

// Compares a TDateTime or span result with the expected value
procedure TestValue(Caption: String; Value, Expected: Double);
begin
  Write(Format('    %s = %.9f, expected: %.9f', [Caption, Value, Expected]));
  if SameValue(Value, Expected, 1E-9) then
    WriteLn
  else
   begin
    WriteLn(' ---> ERROR');
    exitcode:=1;
   end;
end;

// Checks that a zero increment returns the value unchanged
procedure TestRoundTrip(dt: TDateTime);
begin
  TestValue(Format('IncDay(%.6f, 0)', [dt]), IncDay(dt, 0), dt);
  TestValue(Format('IncHour(%.6f, 0)', [dt]), IncHour(dt, 0), dt);
  TestValue(Format('IncDay(IncDay(%.6f, -3), 3)', [dt]), IncDay(IncDay(dt, -3), 3), dt);
end;

var
  dtLow, dtNext: TDateTime;

begin
  exitcode:=0;
  // ---------------------------------------------------------------------------
  // Testing DaysBetween
  // ---------------------------------------------------------------------------
  Writeln('DaysBetween');

  // Crossing zero date
  WriteLn('- Crossing zero date');
  Test('DaysBetween', @DaysBetween, 1, IncDay(1, -2), 2);
  Test('DaysBetween', @DaysBetween, IncDay(1, -2), 1, 2);

  // all positive
  WriteLn('- All dates positive');
  Test('DaysBetween', @DaysBetween, 1, IncDay(1, +2), 2);
  Test('DaysBetween', @DaysBetween, IncDay(1, +2), 1, 2);

  // all negative
  WriteLn('- All dates negative');
  Test('DaysBetween', @DaysBetween, IncDay(1, -10), IncDay(1, -8), 2);
  Test('DaysBetween', @DaysBetween, IncDay(1, -8), IncDay(1, -10), 2);

  // ---------------------------------------------------------------------------
  // Testing HoursBetween
  // ---------------------------------------------------------------------------
  WriteLn;
  WriteLn('HoursBetween');
  // Crossing zero date
  WriteLn('- Crossing zero date');
  Test('HoursBetween', @HoursBetween, 0.25, IncHour(0.25, -2), 2);
  Test('HoursBetween', @HoursBetween, IncHour(0.25, -2), 0.25, 2);

  // all positive
  WriteLn('- All dates positive');
  Test('HoursBetween', @HoursBetween, 0.25, IncHour(0.25, +2), 2);
  Test('HoursBetween', @HoursBetween, IncHour(0.25, +2), 0.25, 2);

  // all negative
  WriteLn('- All dates negative');
  Test('HoursBetween', @HoursBetween, -1.25, IncHour(-1.25, -2), 2);
  Test('HoursBetween', @HoursBetween, IncHour(-1.25, -2), -1.25, 2);

  // ---------------------------------------------------------------------------
  // Testing MinutesBetween
  // ---------------------------------------------------------------------------
  WriteLn;
  WriteLn('MinutesBetween');
  // Crossing zero date
  WriteLn('- Crossing zero date');
  Test('MinutesBetween', @MinutesBetween, 0.25, IncMinute(0.25, -2), 2);
  Test('MinutesBetween', @MinutesBetween, IncMinute(0.25, -2), 0.25, 2);

  // all positive
  WriteLn('- All dates positive');
  Test('MinutesBetween', @MinutesBetween, 0.25, IncMinute(0.25, +2), 2);
  Test('MinutesBetween', @MinutesBetween, IncMinute(0.25, +2), 0.25, 2);

  // all negative
  WriteLn('- All dates negative');
  Test('MinutesBetween', @MinutesBetween, -1.25, IncMinute(-1.25, -2), 2);
  Test('MinutesBetween', @MinutesBetween, IncMinute(-1.25, -2), -1.25, 2);
  Test('MinutesBetween', @MinutesBetween, -0.25, IncMinute(-0.25, -2), 2);
  Test('MinutesBetween', @MinutesBetween, IncMinute(-0.25, -2), -0.25, 2);
  Test('MinutesBetween', @MinutesBetween, -1.0, IncMinute(-1.0, -2), 2);
  Test('MinutesBetween', @MinutesBetween, IncMinute(-1.0, -2), -1.0, 2);

  WriteLn;
  WriteLn('DaysBetween in steps of 2 days across the zero date');
  dtLow := -3.75;
  while dtLow < 3 do
    begin
    dtNext := IncDay(dtLow, 2);
    Test('DaysBetween', @DaysBetween, dtLow, dtNext, 2);
    Test('DaysBetween', @DaysBetween, dtNext, dtLow, 2);
    dtLow := dtNext;
    end;

  WriteLn;
  WriteLn('Round trip of negative date/time values');
  TestRoundTrip(-1.0);
  TestRoundTrip(-1.25);
  TestRoundTrip(-1.5);
  TestRoundTrip(-1.75);
  TestRoundTrip(-2.0);
  TestRoundTrip(-2.5);
  TestRoundTrip(-10.999);
  TestRoundTrip(1.25);

  WriteLn;
  WriteLn('Increments across the zero date');
  TestValue('IncDay(1.25, -3)', IncDay(1.25, -3), -2.25);
  TestValue('IncDay(-2.25, 3)', IncDay(-2.25, 3), 1.25);
  TestValue('IncHour(-1.25, 6)', IncHour(-1.25, 6), -1.5);
  TestValue('IncHour(-1.5, -6)', IncHour(-1.5, -6), -1.25);

  WriteLn;
  WriteLn('Spans with negative date/time values');
  TestValue('DaySpan(-1.25, 0.25)', DaySpan(-1.25, 0.25), 1.0);
  TestValue('DaySpan(-2.75, 1.75)', DaySpan(-2.75, 1.75), 3.0);
  TestValue('DaySpan(-2.0, -1.0)', DaySpan(-2.0, -1.0), 1.0);
  TestValue('HourSpan(-1.25, -1.5)', HourSpan(-1.25, -1.5), 6.0);
  TestValue('HourSpan(-1.75, 0.25)', HourSpan(-1.75, 0.25), 12.0);
end.

